{-# LANGUAGE BangPatterns #-}

module Ir.HighLevelIr
  ( highLevelIr
  , Program
  , ProgramRoots
  , irToString
  , exprToString
  , lookupConst
  , Expr (..)
  , Var
  , Const (..)
  , prConst
  , nameConst
  , Ctor (..)
  , prCtor
  , freeVars
  )
where

import qualified Str
import Data.Maybe (fromJust)
import Data.List (sortOn)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified TypeCheck.Env as Env
import qualified TypeCheck.Term as Te
import qualified TypeCheck.TermEnv as TE
import Control.Monad (when)
import Control.Monad.Trans.State
import Control.Monad.Writer (execWriter, Writer, tell)
import Control.Exception (assert)
import Data.Foldable (foldrM, foldlM)

--import Debug.Trace (trace)

type Var = Int

data Const = Const String Int deriving Show

prConst :: Const -> Int
prConst (Const _ i) = i

nameConst :: Const -> String
nameConst (Const n _) = n

instance Eq Const where
  c1 == c2 = prConst c1 == prConst c2

instance Ord Const where
  c1 <= c2 = prConst c1 <= prConst c2

type CtorId = Int
type FieldCnt = Int

data Ctor = Ctor CtorId deriving Show

prCtor :: Ctor -> CtorId
prCtor (Ctor i) = i

unitCtor :: Ctor
unitCtor = Ctor 0

dataCtor :: Ctor
dataCtor = unitCtor

instance Eq Ctor where
  c1 == c2 = prCtor c1 == prCtor c2

instance Ord Ctor where
  c1 <= c2 = prCtor c1 <= prCtor c2

data Expr =
    Ap Bool Var [Var]
  | Force Bool Var
  | Fun [Var] Expr
  | Extern Int
  | Lazy Bool Expr
  | Match Var [(CtorId, FieldCnt, Expr)]
  | ConstRef Const
  | CtorRef Ctor [Var]
  | Proj Int Var
  | Let Var Expr Expr
  | Ret Var
  | Where Expr ProgramRoots

type Program = IntMap (Const, Expr, Bool) -- Bool = True if static.

type ProgramRoots = [Const]

lookupConst :: Const -> Program -> (Expr, Bool)
lookupConst c p =
  let (_, e, b) = fromJust (IntMap.lookup (prConst c) p) in (e, b)

freeVars :: Program -> Expr -> IntSet
freeVars p = \e -> evalState (freev e) IntSet.empty
  where
    freev :: Expr -> State IntSet IntSet
    freev (Ap _ v vs) =
      return (IntSet.insert v (IntSet.fromList vs))
    freev (Force _ v) = return (IntSet.singleton v)
    freev (Fun vs e) = do
      e' <- freev e
      return (IntSet.difference e' (IntSet.fromList vs))
    freev (Extern _) = return IntSet.empty
    freev (Lazy _ e) = freev e
    freev (Match v cs) = do
      cs' <- foldlM (\a (_, _, x) -> fmap (IntSet.union a) (freev x))
                  IntSet.empty cs
      return (IntSet.insert v cs')
    freev (ConstRef c) = do
      m <- get
      if IntSet.member (prConst c) m
      then return IntSet.empty
      else do
        put (IntSet.insert (prConst c) m)
        let (e, b) = lookupConst c p
        if b then return IntSet.empty else freev e
    freev (CtorRef _ vs) = return (IntSet.fromList vs)
    freev (Proj _ v) = return (IntSet.singleton v)
    freev (Let v e1 e2) = do
      e1' <- freev e1
      e2' <- freev e2
      return (IntSet.difference (IntSet.union e1' e2') (IntSet.singleton v))
    freev (Ret v) = return (IntSet.singleton v)
    freev (Where e w) = do
      e' <- freev e
      w' <- foldlM (\m x -> fmap (IntSet.union m) (freev x))
              IntSet.empty (map (fst . flip lookupConst p) w)
      return (IntSet.union e' w')

data St = St
  { teImplicitMap :: Te.ImplicitMap
  , teRefMap :: Te.RefMap
  , teDataCtorMap :: Te.DataCtorMap
  , program :: Program
  , constMap :: IntMap Const -- Te ref id -> Const
  , varMap :: IntMap Var     -- Te var id -> Var
  , ctorMap :: IntMap Ctor   -- Te ctor id -> Ctor
  , nextConst :: Int
  , nextVar :: Int
  , unitConst :: Const
  }

type StM = State St

highLevelIr ::
  [Te.RefVar] -> Te.DataCtorMap -> Te.ImplicitMap -> Te.RefMap ->
  (Program, ProgramRoots)
highLevelIr = runStM

convertName :: String -> String
convertName n =
  let (n0, n1) = Str.operandSplit n
      n0' = if Str.isInfixOp n0
            then Str.infixOperatorPrefix ++ Str.stripOperatorStr n0
            else
             if Str.isPrefixOp n0
             then Str.prefixOperatorPrefix ++ Str.stripOperatorStr n0
             else
              if Str.isPostfixOp n0
              then Str.postfixOperatorPrefix ++ Str.stripOperatorStr n0
              else n0
  in
    case n1 of
      Nothing -> n0'
      Just n1' ->
        if null n1'
        then n0'
        else n0' ++ Str.operandDelim ++ convertName n1'

getNextConst :: Bool -> String -> StM Const
getNextConst convert n = do
  let n' = if convert then convertName n else n
  st <- get
  let c = nextConst st
  put (st {nextConst = c + 1})
  return (Const n' c)

getNextVar :: StM Var
getNextVar = do
  st <- get
  let i = nextVar st
  put (st {nextVar = i + 1})
  return i

updateTermRef :: Te.Var -> Const -> StM ()
updateTermRef v c =
  modify (\st -> st {constMap = IntMap.insert (Te.varId v) c (constMap st)})

lookupTermRef :: Te.Var -> StM Const
lookupTermRef i =
  fmap (fromJust . IntMap.lookup (Te.varId i) . constMap) get

updateTermCtor :: Te.Var -> Ctor -> StM ()
updateTermCtor v c =
  modify (\st -> st {ctorMap = IntMap.insert (Te.varId v) c (ctorMap st)})

lookupTermCtor :: Te.VarId -> StM Ctor
lookupTermCtor i = do
  fmap (fromJust . IntMap.lookup i . ctorMap) get

updateTermVar :: Te.Var -> Var -> StM ()
updateTermVar v x = do
  modify (\st -> st {varMap = IntMap.insert (Te.varId v) x (varMap st)})

localUpdateTermVars :: [Te.Var] -> [Var] -> StM a -> StM a
localUpdateTermVars vs xs e = do
  m <- fmap varMap get
  let xs' = map (\v -> IntMap.lookup (Te.varId v) m) vs
  mapM_ (uncurry updateTermVar) (zip vs xs)
  e' <- e
  mapM_ (\(v, x) ->
            case x of
              Nothing -> return ()
              Just x' -> updateTermVar v x'
        ) (zip vs xs')
  return e'

lookupTermVar :: Te.Var -> StM Var
lookupTermVar i =
  fmap (fromJust . IntMap.lookup (Te.varId i) . varMap) get

getRefMap :: StM Te.RefMap
getRefMap = fmap teRefMap get

lookupRef :: Te.VarId -> StM Te.Term
lookupRef v = do
  rm <- getRefMap
  return (Env.forceLookupRefMap v rm)

getImplicitMap :: StM Te.ImplicitMap
getImplicitMap = fmap teImplicitMap get

lookupImplicit :: Te.VarId -> StM Te.Implicits
lookupImplicit v = do
  im <- getImplicitMap
  return (Env.forceLookupImplicitMap v im)

getDataCtorMap :: StM Te.DataCtorMap
getDataCtorMap = fmap teDataCtorMap get

lookupDataCtor :: Te.VarId -> StM [Te.Var]
lookupDataCtor v = do
  dm <- getDataCtorMap
  return (Env.forceLookupDataCtorMap v dm)

runStM ::
  [Te.RefVar] -> Te.DataCtorMap -> Te.ImplicitMap -> Te.RefMap ->
  (Program, ProgramRoots)
runStM vs dm im rm =
  let st = St { teImplicitMap = im
              , teRefMap = rm
              , teDataCtorMap = dm
              , program = IntMap.empty
              , constMap = IntMap.empty
              , varMap = IntMap.empty
              , ctorMap = IntMap.empty
              , nextConst = 0
              , nextVar = 0
              , unitConst = Const "" 0 -- Dummy value in the beginning.
              }
      (x, st') = runState (irInitProgram vs) st
  in (program st', x)

irInitProgram :: [Te.RefVar] -> StM ProgramRoots
irInitProgram vs = do
  c <- getNextConst False Str.unitName
  modify (\st -> st {unitConst = c})
  rs <- irProgram True vs
  p <- fmap program get
  let r = CtorRef unitCtor []
  modify (\st -> st {program = IntMap.insert (prConst c) (c, r, True) p})
  return (c : rs)

irProgram :: Bool -> [Te.RefVar] -> StM ProgramRoots
irProgram isStatic vs = do
  mapM_ updateRefMap vs
  ps <- doIrProgram vs
  st <- get
  p <- fmap program get
  let p' = foldl (\m (c,x) -> IntMap.insert (prConst c) (c, x, isStatic) m) p ps
  put (st {program = p'})
  return (map fst ps)

doIrProgram :: [Te.RefVar] -> StM [(Const, Expr)]
doIrProgram [] = return []
doIrProgram (Te.RefExtern v a : vs) = do
  m <- doIrProgram vs
  c <- lookupTermRef v
  return ((c, Extern a) : m)
doIrProgram (Te.RefVal v : vs) = do
  e <- irRef v
  m <- doIrProgram vs
  c <- lookupTermRef v
  return ((c, e) : m)
doIrProgram (Te.RefData dv : vs) = do
  e <- makeCtorConstFromVar dv dataCtor
  ctors <- lookupDataCtor (Te.varId dv)
  ctors' <- mapM irCtor ctors
  m <- doIrProgram vs
  c <- lookupTermRef dv
  return ((c, e) : ctors' ++ m)
  where
    irCtor :: Te.Var -> StM (Const, Expr)
    irCtor v = do
      ctor <- lookupTermCtor (Te.varId v)
      e <- makeCtorConstFromVar v ctor
      c <- lookupTermRef v
      return (c, e)
doIrProgram (Te.RefCtor _ : vs) = doIrProgram vs

updateRefMap :: Te.RefVar -> StM ()
updateRefMap (Te.RefExtern v _) =
  getNextConst True (Te.varName v) >>= updateTermRef v
updateRefMap (Te.RefVal v) =
  getNextConst True (Te.varName v) >>= updateTermRef v
updateRefMap (Te.RefData v) = do
  t <- lookupRef (Te.varId v)
  case Te.termPre t of
    Te.TermData dv -> do
      cs <- lookupDataCtor (Te.varId dv)
      getNextConst True (Te.varName dv) >>= updateTermRef dv
      mapM_ updateCtor (zip cs [0..])
    _ -> error "expected data from RefData"
  where
    updateCtor :: (Te.Var, Int) -> StM ()
    updateCtor (c, i) = do
      getNextConst True (Te.varName c) >>= updateTermRef c
      updateTermCtor c (Ctor i)
updateRefMap (Te.RefCtor _) = return ()

isRelevantTy :: Te.RefMap -> Te.PreTerm -> Bool
isRelevantTy rm t = isRelevantTyNormalized (TE.preTermNormalize rm t)

isRelevantTyNormalized :: Te.PreTerm -> Bool
isRelevantTyNormalized (Te.TermArrow True _ _) = True
isRelevantTyNormalized (Te.TermArrow False _ c) = isRelevantTyNormalized c
isRelevantTyNormalized (Te.TermLazyArrow True _) = True
isRelevantTyNormalized (Te.TermLazyArrow False c) = isRelevantTyNormalized c
isRelevantTyNormalized Te.TermTy = False
isRelevantTyNormalized _ = True

makeFunVars :: Int -> StM [Var]
makeFunVars 0 = return []
makeFunVars i = do
  v <- getNextVar
  vs <- makeFunVars (i - 1)
  return (v:vs) 

data RVar = Relevant Var | Irrelevant Var

prRVar :: RVar -> Var
prRVar (Relevant v) = v
prRVar (Irrelevant v) = v

makeFunRVars :: (a -> Te.PreTerm) -> [a] -> StM [RVar]
makeFunRVars _ [] = return []
makeFunRVars f (x : xs) = do
  v <- getNextVar
  vs <- makeFunRVars f xs
  rm <- getRefMap
  if isRelevantTy rm (f x)
  then return (Relevant v : vs)
  else return (Irrelevant v : vs)

makeIrrelevantFunNormalized :: Te.PreTerm -> StM Expr
makeIrrelevantFunNormalized t = do
  rm <- getRefMap
  case TE.preTermDomCod rm t of
    Nothing ->
      case TE.preTermLazyCod rm t of
        Nothing -> return (CtorRef dataCtor [])
        Just (c, io) -> do
          e <- makeIrrelevantFunNormalized c
          return (Lazy io e)
    Just (d, c, _) -> do
      vs <- makeFunVars (length d)
      e <- makeIrrelevantFunNormalized c
      return (Fun vs e)

filterRelevantRVars :: [RVar] -> [Var]
filterRelevantRVars [] = []
filterRelevantRVars (Relevant v : vs) = v : filterRelevantRVars vs
filterRelevantRVars (Irrelevant _ : vs) = filterRelevantRVars vs

makeCtorConstFromVar :: Te.Var -> Ctor -> StM Expr
makeCtorConstFromVar v ctor = do
  t <- lookupRef (Te.varId v)
  is <- lookupImplicit (Te.varId v)
  if null is
    then makeCtorConst ctor (Te.termTy t)
    else
      let as = map (\(i,x) -> (Just i, x)) is
      in makeCtorConst ctor (Te.TermArrow False as (Te.termTy t))

makeCtorConst :: Ctor -> Te.PreTerm -> StM Expr
makeCtorConst ctor = make []
  where
    make :: [[Var]] -> Te.PreTerm -> StM Expr
    make as t = do
      rm <- getRefMap
      case TE.preTermDomCod rm t of
        Nothing ->
          case TE.preTermLazyCod rm t of
            Nothing -> return (CtorRef ctor (concat (reverse as)))
            Just (cod, _) -> fmap (Lazy False) (make as cod)
        Just (dom, cod, _) -> do
          a <- makeFunRVars snd dom
          c <- make (filterRelevantRVars a : as) cod
          return (Fun (map prRVar a) c)

irRef :: Te.Var -> StM Expr
irRef v = do
  r <- lookupRef (Te.varId v)
  doIrRef r
  where
    doIrRef :: Te.Term -> StM Expr
    doIrRef t = do
      w <- irProgram False (Te.termNestedDefs t)
      e <- irExpr (Te.termPre t)
      if null w
      then return e
      else return (Where e w)

irFun :: Int -> Maybe Int -> Te.CaseTree -> StM Expr
irFun 0 Nothing _ = error "function without arguments"
irFun n Nothing ct = do
  vs <- makeFunVars n
  fmap (Fun vs) (irCaseTree vs ct)
irFun 0 (Just n) ct = do
  vs <- makeFunVars n
  fmap (Fun vs) (irCaseTree vs ct)
irFun n1 (Just n2) ct = do
  vs1 <- makeFunVars n1
  vs2 <- makeFunVars n2
  fmap (Fun vs1 . Fun vs2) (irCaseTree (vs1 ++ vs2) ct)

irAppExpr ::
  Bool -> Bool -> Te.PreTerm -> [Te.PreTerm] -> StM Expr
irAppExpr isImplicit io f as = do
  v <- getNextVar
  f' <- irExpr f
  ts <- argTypes
  let !() = assert (length as == length ts) ()
  (vs, p) <- foldrM addArg ([], id) (zip as ts)
  return (Let v f' (p (Ap io v vs)))
  where
    addArg ::
      (Te.PreTerm, Maybe Te.PreTerm) ->
      ([Var], Expr -> Expr) ->
      StM ([Var], Expr -> Expr)
    addArg (b, t) (vs, p) = do
      v' <- getNextVar
      b' <- irArg b t
      return (v' : vs, Let v' b' . p)

    irArg :: Te.PreTerm -> Maybe Te.PreTerm -> StM Expr
    irArg b Nothing = irExpr b
    irArg b (Just t) = do
      rm <- getRefMap
      let t' = TE.preTermNormalize rm t
      if isRelevantTyNormalized t'
      then irExpr b
      else makeIrrelevantFunNormalized t'

    argTypes :: StM [Maybe Te.PreTerm]
    argTypes = do
      t0 <- getTypeFromRef f
      case t0 of
        Nothing -> return (map (\_ -> Nothing) as)
        Just t -> argTypesFromType t

    getTypeFromRef :: Te.PreTerm -> StM (Maybe Te.PreTerm)
    getTypeFromRef (Te.TermApp False f' _) = do
      t0 <- getTypeFromRef f'
      case t0 of
        Nothing -> return Nothing
        Just (Te.TermArrow _ _ c) -> return (Just c)
        Just _ -> error "application of non-function"
    getTypeFromRef (Te.TermImplicitApp False f' _) = getTypeFromRef f'
    getTypeFromRef (Te.TermRef r _) = do
      rm <- getRefMap
      case IntMap.lookup (Te.varId r) rm of
        Nothing -> return Nothing
        Just (r', _) -> do
          t <- if not isImplicit
               then return (Te.termTy r')
               else do
                is <- lookupImplicit (Te.varId r)
                let is' = map (\(x, y) -> (Just x, y)) is
                return (Te.TermArrow False is' (Te.termTy r'))
          return (Just (TE.preTermNormalize rm t))
    getTypeFromRef _ = return Nothing

    argTypesFromType :: Te.PreTerm -> StM [Maybe Te.PreTerm]
    argTypesFromType (Te.TermArrow _ ts _) = return (map (Just . snd) ts)
    argTypesFromType _ = return (map (\_ -> Nothing) as)

irExpr :: Te.PreTerm -> StM Expr
irExpr (Te.TermFun ias _ n ct) = irFun (length ias) n ct
irExpr (Te.TermLazyFun io t) = fmap (Lazy io) (irExpr t)
irExpr (Te.TermArrow _ _ _) = do
  c <- fmap unitConst get
  return (ConstRef c)
irExpr (Te.TermLazyArrow _ _) = do
  c <- fmap unitConst get
  return (ConstRef c)
irExpr (Te.TermApp io f as) = irAppExpr False io f as
irExpr (Te.TermImplicitApp _ f as) = irAppExpr True False f (map snd as)
irExpr (Te.TermLazyApp io f) = do
  v <- getNextVar
  f' <- irExpr f
  return (Let v f' (Force io v))
irExpr (Te.TermRef v _) = do
  v' <- lookupTermRef v
  return (ConstRef v')
irExpr (Te.TermData v) = do
  v' <- lookupTermRef v
  return (ConstRef v')
irExpr (Te.TermCtor v _) = do
  v' <- lookupTermRef v
  return (ConstRef v')
irExpr (Te.TermVar _ v) = do
  v' <- lookupTermVar v
  return (Ret v')
irExpr (Te.TermMatch e ct) = do
  v <- getNextVar
  e' <- irExpr e
  ct' <- irCaseTree [v] ct
  return (Let v e' ct')
irExpr Te.TermUnitElem = do
  c <- fmap unitConst get
  return (ConstRef c)
irExpr Te.TermUnitTy = do
  c <- fmap unitConst get
  return (ConstRef c)
irExpr Te.TermTy = do
  c <- fmap unitConst get
  return (ConstRef c)
irExpr Te.TermEmpty = error "cannot generate empty term!"

irCaseTree :: [Var] -> Te.CaseTree -> StM Expr
irCaseTree xs (Te.CaseLeaf vs _ te ws) = do
  let !() = assert (length xs == length vs) ()
  localUpdateTermVars vs xs $ do
    w <- irProgram False ws
    e <- irExpr te
    if null w
      then return e
      else return (Where e w)
irCaseTree xs (Te.CaseNode idx m d) = do
  let (x, xs') = removeIdx idx xs
  let ctors = IntMap.toList m
  cs0 <- mapM (makeCtorCase True x xs') ctors
  let cs = map snd cs0
  allCtors <- lookupDataCtor (fst (head cs0))
  let coveredCtors = IntMap.keysSet m
  let missingCtors = foldr (\v a ->
                              if IntSet.member (Te.varId v) coveredCtors
                              then a
                              else Te.varId v : a
                           ) [] allCtors
  de <- case d of
          Nothing -> return []
          Just (vs, ct) -> do
            let as = map (\i -> (i, (vs, ct))) missingCtors
            c <- mapM (makeCtorCase False x xs') as
            return (map snd c)
  let allCases = sortOn (\(i, _, _) -> i) (cs ++ de)
  return (Match x allCases)
irCaseTree xs (Te.CaseUnit idx (vs, ct)) = do
  let (x, xs') = removeIdx idx xs
  localUpdateTermVars vs (repeat x) $ do
    ct' <- irCaseTree xs' ct
    return (Match x [(0, 0, ct')])
irCaseTree xs (Te.CaseEmpty idx) = do
  let (x, _) = removeIdx idx xs
  return (Match x [])

makeCtorCase ::
  Bool -> Var -> [Var] -> (Te.VarId, ([Te.Var], Te.CaseTree)) ->
  StM (Te.VarId, (CtorId, FieldCnt, Expr))
makeCtorCase makeProjects x xs' (i, (vs, ct)) = do
  localUpdateTermVars vs (repeat x) $ do
    t <- lookupRef i
    rm <- getRefMap
    i' <- lookupTermCtor i
    is <- lookupImplicit i
    let dataType = TE.preTermCodRootType rm (Te.termTy t)
    let dataId = case dataType of
                  Just (Te.TermData dv, _) -> dv
                  _ -> error "expected type of ctor to be a data type"
    case concatDom rm is (Te.termTy t) of
      Nothing -> do
        c <- irCaseTree xs' ct
        return (Te.varId dataId, (prCtor i', 0, c))
      Just dom ->
        if makeProjects
        then do
          ys0 <- makeFunRVars snd dom
          (ys, p) <- makeProjs 0 dataId (zip ys0 dom)
          c <- irCaseTree (ys ++ xs') ct
          let dom' = filter (\(_, dty) -> isRelevantTy rm dty) dom
          return (Te.varId dataId, (prCtor i', length dom', p c))
        else do
          c <- irCaseTree xs' ct
          let dom' = filter (\(_, dty) -> isRelevantTy rm dty) dom
          return (Te.varId dataId, (prCtor i', length dom', c))
  where
    makeProjs ::
      Int -> Te.Var -> [(RVar, (a, Te.PreTerm))] ->
      StM ([Var], Expr -> Expr)
    makeProjs _ _ [] = return ([], id)
    makeProjs n dataId ((Irrelevant y', (_, t)) : ys) = do
      (ys', p) <- makeProjs n dataId ys
      dataVal <- makeIrrelevantFunNormalized t
      return (y' : ys', Let y' dataVal . p)
    makeProjs n dataId ((Relevant y', _) : ys) = do
      (ys', p) <- makeProjs (n + 1) dataId ys
      return (y' : ys', Let y' (Proj n x) . p)

    concatDom ::
      Te.RefMap -> Te.Implicits -> Te.PreTerm ->
      Maybe [(Maybe Te.Var, Te.PreTerm)]
    concatDom rm [] ty = concatDom' rm ty
    concatDom rm is ty =
      let is' = map (\(v,t) -> (Just v, t)) is
      in case concatDom' rm ty of
          Nothing -> return is'
          Just d -> return (is' ++ d)

    concatDom' ::
      Te.RefMap -> Te.PreTerm -> Maybe [(Maybe Te.Var, Te.PreTerm)]
    concatDom' rm t =
      case TE.preTermDomCod rm t of
        Nothing ->
          case TE.preTermLazyCod rm t of
            Nothing -> Nothing
            Just (t', _) -> concatDom' rm t'
        Just (d, c, _) ->
          case concatDom' rm c of
            Nothing -> Just d
            Just d' -> Just (d ++ d')

removeIdx :: Int -> [a] -> (a, [a])
removeIdx 0 (x : xs) = (x, xs)
removeIdx i (x : xs) =
  let (x', xs') = removeIdx (i-1) xs in (x', x : xs')
removeIdx _ _ = error "invalid index to removeIdx IR"

------------------- Printing -----------------------------

irToString :: Program -> ProgramRoots -> String
irToString p r =
  execWriter (runStateT (writeProgram p r) 0)

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

writeProgram :: Program -> ProgramRoots -> ToString ()
writeProgram p = writeConsts
  where
    writeConsts :: ProgramRoots -> ToString ()
    writeConsts [] = return ()
    writeConsts [c] = writeConst c (fst (lookupConst c p))
    writeConsts (c : cs) = do
      writeConsts [c]
      newLine
      newLine
      writeConsts cs

    writeConst :: Const -> Expr -> ToString ()
    writeConst c e = do
      writeStr "val "
      writeStr (nameConst c)
      writeStr " => "
      writeExpr True p e

writeVar :: Var -> ToString ()
writeVar v = writeStr ("x_" ++ show v)

writeVars :: [Var] -> ToString ()
writeVars [] = return ()
writeVars [v] = writeVar v
writeVars (v : vs) = writeVar v >> writeStr ", " >> writeVars vs

writeParenVars :: [Var] -> ToString ()
writeParenVars vs = writeStr "(" >> writeVars vs >> writeStr ")"

isLetExpr :: Expr -> Bool
isLetExpr (Let _ _ _) = True
isLetExpr _ = False

isLetOrCaseExpr :: Expr -> Bool
isLetOrCaseExpr (Let _ _ _) = True
isLetOrCaseExpr (Match _ _) = True
isLetOrCaseExpr _ = False

exprToString :: Program -> Expr -> String
exprToString p e = 
  execWriter (runStateT (writeExpr True p e) 0)

writeExpr :: Bool -> Program -> Expr -> ToString ()
writeExpr _ _ (Ap _ v vs) = writeVar v >> writeParenVars vs
writeExpr _ _ (Force _ v) = writeVar v >> writeStr "[]"
writeExpr _ p (Fun vs e) = do
  writeParenVars vs >> writeStr ". "
  when (isLetExpr e) (writeStr "(")
  writeExpr True p e
  when (isLetExpr e) (writeStr ")")
writeExpr _ _ (Extern a) = writeStr ("extern " ++ show a)
writeExpr _ p (Lazy _ e) = writeStr "[]. " >> writeExpr True p e
writeExpr inci p (Match v cs) = do
  when inci incIndent
  newLine
  writeStr "match "
  writeVar v
  writeCases cs
  newLine
  writeStr "end"
  when inci decIndent
  where
    writeCases :: [(CtorId, FieldCnt, Expr)] -> ToString ()
    writeCases [] = return ()
    writeCases ((i, n, w) : ws) = do
      newLine
      writeStr "let "
      writeStr (show i)
      writeStr "("
      writeStr (show n)
      writeStr ")"
      writeStr " => "
      writeExpr True p w
      writeCases ws
writeExpr _ _ (ConstRef c) = writeStr (nameConst c)
writeExpr _ _ (CtorRef c vs) = do
  writeStr "ctor_"
  writeStr (show (prCtor c))
  when (not (null vs)) (writeParenVars vs)
writeExpr _ _ (Proj i v) = do
  writeStr "proj_"
  writeStr (show i)
  writeStr "("
  writeVar v
  writeStr ")"
writeExpr inci p (Let v e1 e2) = do
  when inci incIndent
  newLine
  --writeStr "let "
  writeVar v
  writeStr " := "
  writeExpr True p e1
  writeStr ";"
  when (not (isLetOrCaseExpr e2)) newLine
  writeExpr False p e2
  when inci decIndent
writeExpr _ _ (Ret v) = writeVar v
writeExpr inci p (Where e w) = do
  writeExpr inci p e
  newLine
  writeStr "where"
  incIndent
  newLine
  writeProgram p w
  decIndent
  newLine
  writeStr "end"
