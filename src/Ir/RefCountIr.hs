{-# LANGUAGE BangPatterns #-}

module Ir.RefCountIr
  ( Var
  , Const (..)
  , prConst
  , nameConst
  , Ctor (..)
  , prCtor
  , CtorId
  , Ref (..)
  , LetExpr (..)
  , FunExpr (..)
  , ConstExpr (..)
  , Program
  , lookupProgram
  , refCountIr
  , irToString
  , constExprToString
  , funExprToString
  , forceProjArgs
  )
where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad.State
import qualified Ir.BaseIr as B
import Data.Maybe (fromJust, isJust)
import Control.Monad.Writer (Writer, tell, execWriter)
import Control.Exception (assert)

--import Debug.Trace (trace)

type Var = Int

data Const = Const String Int deriving Show

prConst :: Const -> Int
prConst (Const _ i) = i

nameConst :: Const -> String
nameConst (Const n _) = n

convertConst :: B.Const -> Const
convertConst c = Const (B.nameConst c) (B.prConst c)

instance Eq Const where
  c1 == c2 = prConst c1 == prConst c2

instance Ord Const where
  c1 <= c2 = prConst c1 <= prConst c2

data Ctor = Ctor Int deriving Show

prCtor :: Ctor -> Int
prCtor (Ctor i) = i

type CtorId = Int
type FieldCnt = Int

instance Eq Ctor where
  c1 == c2 = prCtor c1 == prCtor c2

instance Ord Ctor where
  c1 <= c2 = prCtor c1 <= prCtor c2

convertCtor :: B.Ctor -> Ctor
convertCtor c = Ctor (B.prCtor c)

data Ref =
    VarRef Var
  | ConstRef Const

sameRef :: Ref -> Ref -> Bool
sameRef (VarRef v1) (VarRef v2) = v1 == v2
sameRef (ConstRef c1) (ConstRef c2) = prConst c1 == prConst c2
sameRef _ _ = False

instance Eq Ref where
  (==) = sameRef

instance Show Ref where
  show (VarRef v) = "x_" ++ show v
  show (ConstRef c) = nameConst c

type LiveSet = IntSet

data PreLetExpr =
    PreAp Bool Ref [Ref]
  | PrePap Const [Ref]
  | PreMkLazy Bool (Var, [Ref]) Const -- Bool and (Var, [Ref]) is just for code gen
  | PreForce Bool Ref (Maybe (Const, [Ref]))
  | PreCtorBox Ctor [Ref]
  | PreProj Int Ref
  | PreReset FieldCnt
  | PreReuse Var Ctor [Ref]

preForceProjArgs :: Maybe (Const, [Ref]) -> [Ref]
preForceProjArgs Nothing = []
preForceProjArgs (Just (_, rs)) = rs

data PreFunExpr =
    PreCase Ref [(CtorId, FieldCnt, PreFunExpr)] LiveSet
  | PreLet Var PreLetExpr PreFunExpr LiveSet
  | PreRet Ref LiveSet
  | PreVarFieldCnt Var FieldCnt PreFunExpr LiveSet
  | PrePforce Const Var [Ref] PreFunExpr LiveSet
  | PreInc Ref PreFunExpr LiveSet
  | PreDec Ref PreFunExpr LiveSet
  | PreUnuse Ref PreFunExpr LiveSet

prLiveSetPreFunExpr :: PreFunExpr -> LiveSet
prLiveSetPreFunExpr (PreCase _ _ x) = x
prLiveSetPreFunExpr (PrePforce _ _ _ _ x) = x
prLiveSetPreFunExpr (PreLet _ _ _ x) = x
prLiveSetPreFunExpr (PreRet _ x) = x
prLiveSetPreFunExpr (PreVarFieldCnt _ _ _ x) = x
prLiveSetPreFunExpr (PreInc _ _ x) = x
prLiveSetPreFunExpr (PreDec _ _ x) = x
prLiveSetPreFunExpr (PreUnuse _ _ x) = x

data LetExpr =
    Ap Bool Ref [Ref]
  | Pap Const [Ref]
  | MkLazy Bool (Var, [Ref]) Const -- Bool and (Var, [Ref]) is just for code gen
  | Force Bool Ref (Maybe (Const, [Ref]))
  | CtorBox Ctor [Ref]
  | Proj Int Ref
  | Reset FieldCnt
  | Reuse Var Ctor [Ref]

forceProjArgs :: Maybe (Const, [Ref]) -> [Ref]
forceProjArgs Nothing = []
forceProjArgs (Just (_, rs)) = rs

data FunExpr =
    Case Ref [(CtorId, FunExpr)]
  | Let Var LetExpr FunExpr
  | Ret Ref
  | Pforce Const Var [Ref] FunExpr
  | Inc Ref FunExpr
  | Dec Ref FunExpr
  | Unuse Ref FunExpr

data ConstExpr =
    Fun [Var] FunExpr
  | Lazy Bool [Var] FunExpr -- Bool is whether Lasy is static, not effect.
  | Extern [Var]
  | Hidden

type Program = IntMap (Const, ConstExpr)

lookupProgram :: Const -> Program -> ConstExpr
lookupProgram c p = snd (fromJust (IntMap.lookup (prConst c) p))

data St = St
  { baseProgram :: B.Program
  , program :: Program
  }

type StM = State St

refCountIr :: B.Program -> Program
refCountIr p =
  let st = St { baseProgram = p
              , program = IntMap.empty
              }
  in program (execState (runRefCountIr (IntMap.elems p)) st)

updateProgram :: Const -> ConstExpr -> StM ()
updateProgram c e = modify (\s -> s {program = insertp s})
  where
    insertp :: St -> Program
    insertp s = IntMap.insert (prConst c) (c, e) (program s)

runRefCountIr :: [(B.Const, B.ConstExpr)] -> StM ()
runRefCountIr [] = return ()
runRefCountIr ((c, e) : es) = do
  e' <- irConstExpr e
  updateProgram (convertConst c) e'
  runRefCountIr es

irConstExpr :: B.ConstExpr -> StM ConstExpr
irConstExpr B.Hidden = return Hidden
irConstExpr (B.Extern a) = return (Extern a)
irConstExpr (B.Fun vs e) = return (Fun vs (irFunExpr vs e))
irConstExpr (B.Lazy iss vs e) = return (Lazy iss vs (irFunExpr vs e))

irFunExpr :: [Var] -> B.FunExpr -> FunExpr
irFunExpr vs e =
  let e0 = irPreFunExpr IntMap.empty IntSet.empty e
      e1 = irResetReuseFunExpr e0
      e2 = irIncDecFunExpr (IntSet.fromList vs) e1
  in irPreToFunExpr e2

type CoveredFieldVars = IntSet
type VarSubst = IntMap B.Var

irPreFunExpr :: VarSubst -> CoveredFieldVars -> B.FunExpr -> PreFunExpr
irPreFunExpr su co (B.Case r cs) =
  let cs' = map (\(i,n,c) -> (i, n, irPreFunExpr su co c)) cs
      s = foldl (\t (_, _, d) ->
                      IntSet.union (prLiveSetPreFunExpr d) t
                ) IntSet.empty cs'
  in PreCase (convertRef su r) cs' (IntSet.union s (liveSetRef su r))
irPreFunExpr su co (B.Let v e1@(B.CtorBox _ rs) e2) =
  let (e1', li1) = irPreLetExpr su e1
      e2' = irPreFunExpr su (IntSet.insert v co) e2
      s = IntSet.difference
            (prLiveSetPreFunExpr e2') (IntSet.singleton v)
      e2'' = irPreFunExprVarFieldCnt co v (length rs) e2'
  in PreLet v e1' e2'' (IntSet.union li1 s)
irPreFunExpr su co (B.Let v0 (B.Pforce c v rs) e2) =
  let li = liveSetRefs su (B.VarRef v : rs)
      su' = IntMap.insert v0 v su
      e2' = irPreFunExpr su' co e2
      s = prLiveSetPreFunExpr e2'
      li' = IntSet.union li s
      !() = assert (not (IntSet.member v0 li')) ()
  in PrePforce (convertConst c) v (convertRefs su rs) e2' li'
irPreFunExpr su co (B.Let v e1 e2) =
  let (e1', li1) = irPreLetExpr su e1
      e2' = irPreFunExpr su co e2
      s = IntSet.difference
            (prLiveSetPreFunExpr e2') (IntSet.singleton v)
  in PreLet v e1' e2' (IntSet.union li1 s)
irPreFunExpr su _ (B.Ret r) =
  PreRet (convertRef su r) (liveSetRef su r)
irPreFunExpr su co (B.VarFieldCnt v n e) =
  let e' = irPreFunExpr su (IntSet.insert v co) e
  in irPreFunExprVarFieldCnt co v n e'

irPreFunExprVarFieldCnt ::
  CoveredFieldVars -> Var -> FieldCnt -> PreFunExpr -> PreFunExpr
irPreFunExprVarFieldCnt co v n e =
  if IntSet.member v co
  then e
  else PreVarFieldCnt v n e (prLiveSetPreFunExpr e)

irPreLetExpr :: VarSubst -> B.LetExpr -> (PreLetExpr, LiveSet)
irPreLetExpr su (B.Ap b r rs) =
  (PreAp b (convertRef su r) (convertRefs su rs), liveSetRefs su (r:rs))
irPreLetExpr su (B.Pap r rs) =
  (PrePap (convertConst r) (convertRefs su rs), liveSetRefs su rs)
irPreLetExpr su (B.MkLazy isForced (v, rs) c) =
  (PreMkLazy isForced (convertVar su v, convertRefs su rs) (convertConst c) , IntSet.empty)
irPreLetExpr su (B.Force b r Nothing) =
  (PreForce b (convertRef su r) Nothing, liveSetRef su r)
irPreLetExpr su (B.Force b r (Just (c, rs))) =
  (PreForce b (convertRef su r) (Just (convertConst c, convertRefs su rs)),
    liveSetRefs su (r:rs))
irPreLetExpr su (B.CtorBox c rs) =
  (PreCtorBox (convertCtor c) (convertRefs su rs), liveSetRefs su rs)
irPreLetExpr su (B.Proj i r) =
  (PreProj i (convertRef su r), liveSetRef su r)
irPreLetExpr _ (B.Pforce _ _ _) =
  error "unexpected Pforce in irPreLetExpr"

convertVar :: VarSubst -> B.Var -> Var
convertVar su v =
  case IntMap.lookup v su of
    Just v' -> v'
    Nothing -> v

convertRef :: VarSubst -> B.Ref -> Ref
convertRef su (B.VarRef v) = VarRef (convertVar su v)
convertRef _ (B.ConstRef c) = ConstRef (convertConst c)

convertRefs :: VarSubst -> [B.Ref] -> [Ref]
convertRefs su rs = map (convertRef su) rs

liveSetRef :: VarSubst -> B.Ref -> LiveSet
liveSetRef su (B.VarRef v) = IntSet.singleton (convertVar su v)
liveSetRef _ _ = IntSet.empty

liveSetRefs :: VarSubst -> [B.Ref] -> LiveSet
liveSetRefs su rs =
  foldl (\s -> IntSet.union s . liveSetRef su) IntSet.empty rs

irResetReuseFunExpr :: PreFunExpr -> PreFunExpr
irResetReuseFunExpr (PreInc _ _ _) =
  error "unexpected PreInc for irResetReuseFunExpr"
irResetReuseFunExpr (PreDec _ _ _) =
  error "unexpected PreDec for irResetReuseFunExpr"
irResetReuseFunExpr (PreUnuse v e li) =
  PreUnuse v (irResetReuseFunExpr e) li
irResetReuseFunExpr (PreCase r cs li) =
  let cs' = map (\(i, n, c) -> (i, n, resetCase n c)) cs
  in PreCase r cs' li
  where
    resetCase :: FieldCnt -> PreFunExpr -> PreFunExpr
    resetCase n c =
      if n > 0
      then case r of
            VarRef v -> irResetFunExpr v n c
            _ -> irResetReuseFunExpr c
      else irResetReuseFunExpr c
irResetReuseFunExpr (PreRet r li) = PreRet r li
irResetReuseFunExpr (PrePforce c v rs e2 li) =
  PrePforce c v rs (irResetReuseFunExpr e2) li
irResetReuseFunExpr (PreLet v e1 e2 li) =
  PreLet v e1 (irResetReuseFunExpr e2) li
irResetReuseFunExpr (PreVarFieldCnt v n e _) =
  if n > 0 then irResetFunExpr v n e else irResetReuseFunExpr e

irResetFunExpr :: Var -> FieldCnt -> PreFunExpr -> PreFunExpr
irResetFunExpr _ _ (PreInc _ _ _) =
  error "unexpected PreInc for irResetFunExpr"
irResetFunExpr _ _ (PreDec _ _ _) =
  error "unexpected PreDec for irResetFunExpr"
irResetFunExpr v0 n0 e@(PreUnuse v e1 li) =
  if IntSet.member v0 li
  then
    let e1' = irResetFunExpr v0 n0 e1
    in PreUnuse v e1' (prLiveSetPreFunExpr e1')
  else irResultFunExprTryReset v0 n0 e li
irResetFunExpr v0 n0 (PreCase r cs _) =
  let cs' = map (\(i, n, c) -> (i, n, resetCase n c)) cs
      li' = foldl (\s (_, _, c) ->
                      IntSet.union (prLiveSetPreFunExpr c) s
                  ) IntSet.empty cs'
  in
    case r of
      VarRef r' -> PreCase r cs' (IntSet.insert r' li')
      _ -> PreCase r cs' li'
  where
    resetCase :: FieldCnt -> PreFunExpr -> PreFunExpr
    resetCase n c =
      if n > 0
      then case r of
            VarRef v -> irResetFunExpr v0 n0 (irResetFunExpr v n c)
            _ -> irResetFunExpr v0 n0 c
      else irResetFunExpr v0 n0 c
irResetFunExpr _ _ (PreRet r li) = PreRet r li
irResetFunExpr v0 n0 e@(PrePforce c v rs e2 li) =
  if IntSet.member v0 li
  then
    let e2' = irResetFunExpr v0 n0 e2
        s = IntSet.union (prLiveSetPreFunExpr e2') li
    in PrePforce c v rs e2' s
  else irResultFunExprTryReset v0 n0 e li
irResetFunExpr v0 n0 e@(PreLet v e1 e2 li) =
  if isVarArgIn v0 e1 || isResetting e1
  then irResetReuseFunExpr e
  else
    if IntSet.member v0 li
    then
      let e2' = irResetFunExpr v0 n0 e2
          s = IntSet.union (prLiveSetPreFunExpr e2') li
      in case e1 of
          PreReset _ -> PreLet v e1 e2' s
          _ -> PreLet v e1 e2' (IntSet.delete v s)
    else irResultFunExprTryReset v0 n0 e li
  where
    isResetting :: PreLetExpr -> Bool
    isResetting (PreReset _) = v == v0
    isResetting _ = False
irResetFunExpr v0 n0 (PreVarFieldCnt v n e _) =
  if n > 0
  then irResetFunExpr v0 n0 (irResetFunExpr v n e)
  else irResetFunExpr v0 n0 e

isVarArgIn :: Var -> PreLetExpr -> Bool
isVarArgIn v (PreAp _ r rs) = isVarArgInRefs v (r : rs)
isVarArgIn v (PrePap _ rs) = isVarArgInRefs v rs
isVarArgIn _ (PreMkLazy _ _ _) = False
isVarArgIn v (PreForce _ _ rs) = isVarArgInRefs v (preForceProjArgs rs)
isVarArgIn v (PreCtorBox _ rs) = isVarArgInRefs v rs
isVarArgIn _ (PreProj _ _) = False
isVarArgIn _ (PreReset _) = False
isVarArgIn v (PreReuse _ _ rs) = isVarArgInRefs v rs

isVarArgInRefs :: Var -> [Ref] -> Bool
isVarArgInRefs _ [] = False
isVarArgInRefs v (r : rs) = isVarArgInRef v r || isVarArgInRefs v rs

isVarArgInRef :: Var -> Ref -> Bool
isVarArgInRef v (VarRef v') = v == v'
isVarArgInRef _ _ = False

irResultFunExprTryReset ::
  Var -> FieldCnt -> PreFunExpr -> LiveSet -> PreFunExpr
irResultFunExprTryReset v0 n0 e li =
  case irReuseFunExpr v0 n0 e of
    Nothing -> irResetReuseFunExpr e
    Just e' ->
      let e'' = irResetReuseFunExpr e'
      in PreLet v0 (PreReset n0) e'' (IntSet.insert v0 li)

irReuseFunExpr :: Var -> FieldCnt -> PreFunExpr -> Maybe PreFunExpr
irReuseFunExpr _ _ (PreInc _ _ _) =
  error "unexpected PreInc for irReuseFunExpr"
irReuseFunExpr _ _ (PreDec _ _ _) =
  error "unexpected PreDec for irReuseFunExpr"
irReuseFunExpr v0 n0 (PreLet v e1@(PreCtorBox c rs) e2 li) =
  if n0 == length rs
  then Just (PreLet v (PreReuse v0 c rs) e2 li)
  else do
    e2' <- irReuseFunExpr v0 n0 e2
    Just (PreLet v e1 e2' li)
irReuseFunExpr v0 n0 (PrePforce c v rs e2 li) = do
  e2' <- irReuseFunExpr v0 n0 e2
  Just (PrePforce c v rs e2' li)
irReuseFunExpr v0 n0 (PreLet v e1 e2 li) = do
  e2' <- irReuseFunExpr v0 n0 e2
  Just (PreLet v e1 e2' li)
irReuseFunExpr _ _ (PreRet _ _) = Nothing
irReuseFunExpr v0 n0 (PreCase r cs li) =
  let cs0 = map (\(i, n, c) -> (i, n, c, irReuseFunExpr v0 n0 c)) cs
      hasReuse = any (\(_, _, _, c0) -> isJust c0) cs0
      cs' = map (\(i, n, c, c0) -> (i, n, chooseCase c c0)) cs0
  in if hasReuse
      then Just (PreCase r cs' li)
      else Nothing
  where
    chooseCase :: PreFunExpr -> Maybe PreFunExpr -> PreFunExpr
    chooseCase c Nothing = PreUnuse (VarRef v0) c (prLiveSetPreFunExpr c)
    chooseCase _ (Just c0) = c0
irReuseFunExpr v0 n0 (PreVarFieldCnt v n e li) = do
  e' <- irReuseFunExpr v0 n0 e
  Just (PreVarFieldCnt v n e' li)
irReuseFunExpr v0 n0 (PreUnuse v e li) = do
  e' <- irReuseFunExpr v0 n0 e
  Just (PreUnuse v e' li)

irIncDecFunExpr :: LiveSet -> PreFunExpr -> PreFunExpr
irIncDecFunExpr _ (PreInc _ _ _) = 
  error "unexpected inc instruction in irIncDecFunExpr"
irIncDecFunExpr _ (PreDec _ _ _) =
  error "unexpected dec instruction in irIncDecFunExpr"
irIncDecFunExpr _ (PreVarFieldCnt _ _ _ _) =
  error "expected PreVarFieldCnt to not be in irIncDecFunExpr"
irIncDecFunExpr ctx (PreUnuse v e li) =
  let (prefix, ctx') = decDeadVars ctx li
  in prefix (PreUnuse v (irIncDecFunExpr ctx' e) li)
irIncDecFunExpr ctx (PreRet r li) =
  let (prefix1, _ctx') = decDeadVars ctx li
      !() = assert (checkContext _ctx') ()
      prefix2 = case r of
                  VarRef _ -> id
                  r' -> \x -> PreInc r' x li
  in prefix1 (prefix2 (PreRet r li))
  where
    checkContext :: LiveSet -> Bool
    checkContext ctx' =
      case r of
        VarRef r' -> ctx' == IntSet.singleton r'
        _ -> IntSet.null ctx'
irIncDecFunExpr ctx (PreCase r cs li) =
  let (prefix, ctx') = decDeadVars ctx li
  in prefix (PreCase r (map (incDecCase ctx') cs) li)
  where
    incDecCase ::
      LiveSet -> (CtorId, FieldCnt, PreFunExpr) ->
      (CtorId, FieldCnt, PreFunExpr)
    incDecCase ctx' (i, n, c) = (i, n, irIncDecFunExpr ctx' c)
irIncDecFunExpr ctx (PrePforce pc v rs e2 lli) =
  let (prefix1, ctx') = decDeadVars ctx lli
      li = prLiveSetPreFunExpr e2
      rs' = filter (/= VarRef v) rs
      (prefix2, s) = incApRefs lli li rs'
      ctx'' = IntSet.difference ctx' s
      e2' = irIncDecFunExpr (IntSet.insert v ctx'') e2
  in prefix1 (prefix2 (PrePforce pc v rs e2' lli))
irIncDecFunExpr ctx (PreLet v e1@(PreAp _ r rs) e2 lli) =
  irIncDecLetExprAp ctx v e1 e2 (r : rs) lli
irIncDecFunExpr ctx (PreLet v e1@(PrePap _ rs) e2 lli) =
  irIncDecLetExprAp ctx v e1 e2 rs lli
irIncDecFunExpr ctx (PreLet v e1@(PreForce _ _ rs) e2 lli) =
  irIncDecLetExprAp ctx v e1 e2 (preForceProjArgs rs) lli
irIncDecFunExpr ctx (PreLet v e1@(PreCtorBox _ rs) e2 lli) =
  irIncDecLetExprAp ctx v e1 e2 rs lli
irIncDecFunExpr ctx (PreLet v e1@(PreMkLazy _ _ _) e2 lli) =
  irIncDecLetExprAp ctx v e1 e2 [] lli
irIncDecFunExpr ctx (PreLet v e1@(PreProj _ _) e2 lli) =
  let li = prLiveSetPreFunExpr e2
      (prefix, ctx') = decDeadVars ctx lli
      e2' = irIncDecFunExpr (IntSet.insert v ctx') e2
  in prefix (PreLet v e1 (PreInc (VarRef v) e2' (IntSet.insert v li)) lli)
irIncDecFunExpr ctx (PreLet v e1@(PreReset _) e2 lli) =
  let (prefix, ctx') = decDeadVars ctx lli
      e2' = irIncDecFunExpr (IntSet.delete v ctx') e2
  in prefix (PreLet v e1 e2' lli)
irIncDecFunExpr ctx (PreLet v e1@(PreReuse _ _ rs) e2 lli) =
  irIncDecLetExprAp ctx v e1 e2 rs lli

irIncDecLetExprAp ::
  LiveSet -> Var -> PreLetExpr -> PreFunExpr -> [Ref] -> LiveSet -> PreFunExpr
irIncDecLetExprAp ctx v e1 e2 rs lli =
  let (prefix1, ctx') = decDeadVars ctx lli
      li = prLiveSetPreFunExpr e2
      (prefix2, s) = incApRefs lli li rs
      ctx'' = IntSet.difference ctx' s
      e2' = irIncDecFunExpr (IntSet.insert v ctx'') e2
  in prefix1 (prefix2 (PreLet v e1 e2' lli))

incApRefs ::
  LiveSet -> LiveSet -> [Ref] ->
  (PreFunExpr -> PreFunExpr, LiveSet)
incApRefs _ _ [] = (id, IntSet.empty)
incApRefs lli li (VarRef v : rs) =
  let li0 = IntSet.union li (refsToLiveSet rs)
  in if IntSet.member v li0
      then let (f, s) = incApRefs lli li rs
           in (\x -> PreInc (VarRef v) (f x) lli, s)
      else let (f, s) = incApRefs lli li rs
           in (f, IntSet.insert v s)
incApRefs lli li (r : rs) =
  let (f, s) = incApRefs lli li rs in (\x -> PreInc r (f x) lli, s)

refsToLiveSet :: [Ref] -> LiveSet
refsToLiveSet [] = IntSet.empty
refsToLiveSet (VarRef v : rs) = IntSet.insert v (refsToLiveSet rs)
refsToLiveSet (_ : rs) = refsToLiveSet rs

decDeadVars :: LiveSet -> LiveSet -> (PreFunExpr -> PreFunExpr, LiveSet)
decDeadVars ctx li =
  let (ctx', vs) = IntSet.partition (flip IntSet.member li) ctx
      decs = IntSet.foldr (\v f x -> PreDec (VarRef v) (f x) li) id vs
  in (decs, ctx')

irPreToFunExpr :: PreFunExpr -> FunExpr
irPreToFunExpr (PreInc v e _) = Inc v (irPreToFunExpr e)
irPreToFunExpr (PreDec v e _) = Dec v (irPreToFunExpr e)
irPreToFunExpr (PreUnuse v e _) = Unuse v (irPreToFunExpr e)
irPreToFunExpr (PreVarFieldCnt _ _ _ _) =
  error "expected PreVarFieldCnt to not be here"
irPreToFunExpr (PreRet r _) = Ret r
irPreToFunExpr (PrePforce pc v rs e2 _) =
  Pforce pc v rs (irPreToFunExpr e2)
irPreToFunExpr (PreLet v e1 e2 _) =
  Let v (irPreToLetExpr e1) (irPreToFunExpr e2)
irPreToFunExpr (PreCase r cs _) =
  Case r (map (\(i, _, c) -> (i, irPreToFunExpr c)) cs)

irPreToLetExpr :: PreLetExpr -> LetExpr
irPreToLetExpr (PreAp b r rs) = Ap b r rs
irPreToLetExpr (PrePap r rs) = Pap r rs
irPreToLetExpr (PreMkLazy isForced lp c) = MkLazy isForced lp c
irPreToLetExpr (PreForce b r rs) = Force b r rs
irPreToLetExpr (PreCtorBox c rs) = CtorBox c rs
irPreToLetExpr (PreProj i r) = Proj i r
irPreToLetExpr (PreReset n) = Reset n
irPreToLetExpr (PreReuse v c rs) = Reuse v c rs

----------------- Printing ---------------------

irToString :: Program -> String
irToString p =
  execWriter (runStateT (writeProgram p) 0)

type ToString a = StateT Int (Writer String) a

incIndent :: ToString ()
incIndent = modify (\n -> n+2)

decIndent :: ToString ()
decIndent = modify (\n -> n-2)

writeIndent :: ToString ()
writeIndent = get >>= doWrite
  where
    doWrite :: Int -> ToString ()
    doWrite 0 = return ()
    doWrite i = tell " " >> doWrite (i - 1)

writeStr :: String -> ToString ()
writeStr s = tell s

newLine :: ToString ()
newLine = tell "\n" >> writeIndent

writeVar :: Var -> ToString ()
writeVar v = writeStr ("x_" ++ show v)

writeRef :: Ref -> ToString ()
writeRef (VarRef v) = writeVar v
writeRef (ConstRef c) = writeStr (nameConst c)

writeVars :: [Var] -> ToString ()
writeVars [] = return ()
writeVars [v] = writeVar v
writeVars (v : vs) = writeVar v >> writeStr ", " >> writeVars vs

writeRefs :: [Ref] -> ToString ()
writeRefs [] = return ()
writeRefs [v] = writeRef v
writeRefs (v : vs) = writeRef v >> writeStr ", " >> writeRefs vs

writeCurlyRefs :: [Ref] -> ToString ()
writeCurlyRefs vs = writeStr "{" >> writeRefs vs >> writeStr "}"

writeSquareVars :: [Var] -> ToString ()
writeSquareVars vs = writeStr "[" >> writeVars vs >> writeStr "]"

writeSquareRefs :: [Ref] -> ToString ()
writeSquareRefs vs = writeStr "[" >> writeRefs vs >> writeStr "]"

writeParenVars :: [Var] -> ToString ()
writeParenVars vs = writeStr "(" >> writeVars vs >> writeStr ")"

writeParenRefs :: [Ref] -> ToString ()
writeParenRefs vs = writeStr "(" >> writeRefs vs >> writeStr ")"

writeProgram :: Program -> ToString ()
writeProgram p = writeConsts (IntMap.elems p)
  where
    writeConsts :: [(Const, ConstExpr)] -> ToString ()
    writeConsts [] = return ()
    writeConsts [(c, e)] = writeConst c e
    writeConsts ((c, e) : cs) = do
      writeConsts [(c, e)]
      newLine
      newLine
      writeConsts cs

constExprToString :: Const -> ConstExpr -> String
constExprToString c e = execWriter (runStateT (writeConst c e) 0)

writeConst :: Const -> ConstExpr -> ToString ()
writeConst c Hidden = do
  writeStr "hidden "
  writeStr (nameConst c)
writeConst c (Extern vs) = do
  writeStr "extern "
  writeStr (nameConst c)
  writeParenVars vs
writeConst c (Fun vs e) = do
  writeStr "val "
  writeStr (nameConst c)
  writeParenVars vs
  writeStr " => "
  writeFunExpr True e
writeConst c (Lazy _ vs e) = do
  writeStr "val "
  writeStr (nameConst c)
  writeSquareVars vs
  writeStr " => "
  writeFunExpr True e

funExprToString :: FunExpr -> String
funExprToString e = execWriter (runStateT (writeFunExpr False e) 0)

writeFunExpr :: Bool -> FunExpr -> ToString ()
writeFunExpr _ (Ret v) = writeRef v
writeFunExpr b (Pforce _ v vs e2) = do
  when b (incIndent >> newLine)
  writeVar v >> writeStr "[{" >> writeRefs vs >> writeStr "}];"
  newLine
  writeFunExpr False e2
  when b decIndent
writeFunExpr b (Let v e1 e2) = do
  when b (incIndent >> newLine)
  writeVar v
  writeStr " := "
  writeLetExpr e1
  writeStr ";"
  newLine
  writeFunExpr False e2
  when b decIndent
writeFunExpr b (Case v cs) = do
  when b (incIndent >> newLine)
  writeStr "case "
  writeRef v
  writeCases cs
  newLine
  writeStr "end"
  when b decIndent
  where
    writeCases :: [(CtorId, FunExpr)] -> ToString ()
    writeCases [] = return ()
    writeCases ((i, w) : ws) = do
      newLine
      writeStr "of "
      writeStr (show i)
      writeStr " => "
      writeFunExpr True w
      writeCases ws
writeFunExpr b (Inc v e) = do
  when b (incIndent >> newLine)
  writeStr "inc("
  writeRef v
  writeStr ");"
  newLine
  writeFunExpr False e
  when b decIndent
writeFunExpr b (Dec v e) = do
  when b (incIndent >> newLine)
  writeStr "dec("
  writeRef v
  writeStr ");"
  newLine
  writeFunExpr False e
  when b decIndent
writeFunExpr b (Unuse v e) = do
  when b (incIndent >> newLine)
  writeStr "unuse("
  writeRef v
  writeStr ");"
  newLine
  writeFunExpr False e
  when b decIndent

writeLetExpr :: LetExpr -> ToString ()
writeLetExpr (Ap _ v vs) = writeRef v >> writeParenRefs vs
writeLetExpr (Pap c vs) = writeStr (nameConst c) >> writeCurlyRefs vs
writeLetExpr (MkLazy _ _ c) =
  writeStr "new-lazy(" >> writeStr (nameConst c) >> writeStr ")"
writeLetExpr (Force _ v vs) =
  writeRef v >> writeSquareRefs (forceProjArgs vs)
writeLetExpr (CtorBox c vs) = do
  writeStr "ctor_"
  writeStr (show (prCtor c))
  when (not (null vs)) (writeParenRefs vs)
writeLetExpr (Proj i v) = do
  writeStr "proj_"
  writeStr (show i)
  writeStr "("
  writeRef v
  writeStr ")"
writeLetExpr (Reset n) = writeStr "reset_" >> writeStr (show n)
writeLetExpr (Reuse v c rs) = do
  writeStr "reuse("
  writeVar v
  writeStr "){"
  writeStr "ctor_"
  writeStr (show (prCtor c))
  when (not (null rs)) (writeParenRefs rs)
  writeStr "}"
