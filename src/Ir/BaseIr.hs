{-# LANGUAGE BangPatterns #-}

module Ir.BaseIr
  ( baseIr
  , Program
  , irToString
  , funExprToString
  , constExprToString
  , LetExpr (..)
  , FunExpr (..)
  , ConstExpr (..)
  , Var
  , Ref (..)
  , Const (..)
  , prConst
  , nameConst
  , Ctor (..)
  , FieldCnt
  , CtorId
  , prCtor
  , freeVarsConstExpr
  , freeVarsFunExpr
  , freeVarsLetExpr
  , sameRef
  )
where

import qualified Str
import Data.Maybe (fromJust, isNothing)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad.State
import Control.Monad.Writer (Writer, tell, execWriter)
import qualified Ir.HighLevelIr as Hl
import Data.Foldable (foldlM, foldrM)
import Control.Exception (assert)

import Debug.Trace (trace)

_useTrace :: String -> a -> a
_useTrace = trace

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

data Ctor = Ctor Int deriving Show

prCtor :: Ctor -> Int
prCtor (Ctor i) = i

type CtorId = Int
type FieldCnt = Int

instance Eq Ctor where
  c1 == c2 = prCtor c1 == prCtor c2

instance Ord Ctor where
  c1 <= c2 = prCtor c1 <= prCtor c2

data Ref =
    VarRef Var
  | ConstRef Const

instance Show Ref where
  show (VarRef v) = "x_" ++ show v
  show (ConstRef c) = nameConst c

sameRef :: Ref -> Ref -> Bool
sameRef (VarRef v1) (VarRef v2) = v1 == v2
sameRef (ConstRef c1) (ConstRef c2) = prConst c1 == prConst c2
sameRef _ _ = False

instance Eq Ref where
  (==) = sameRef

data LetExpr =
    Ap Bool Ref [Ref]
  | Pap Const [Ref]
  | MkLazy Bool (Var, [Ref]) Const -- Bool and (Var, [Ref]) is just for code gen
  | Force Bool Ref (Maybe (Const, [Ref]))
  | Pforce Const Var [Ref]
  | CtorBox Ctor [Ref]
  | Proj Int Ref

projForceArgs :: Maybe (Const, [Ref]) -> [Ref]
projForceArgs Nothing = []
projForceArgs (Just (_, rs)) = rs

data FunExpr =
    Case Ref [(CtorId, FieldCnt, FunExpr)]
  | Let Var LetExpr FunExpr
  | Ret Ref
    -- The VarFieldCnt is used when removing case expressions
    -- with just one case. Then a VarFieldCnt is inserted to
    -- save the size of the variable. This purely meta-data,
    -- no computation.
  | VarFieldCnt Var FieldCnt FunExpr

data ConstExpr =
    Fun [Var] FunExpr
  | Lazy Bool [Var] FunExpr -- Bool is indicating whether it is static, not effect.
  | Extern [Var]

freeVarsRef :: Ref -> IntSet
freeVarsRef (VarRef v) = IntSet.singleton v
freeVarsRef _ = IntSet.empty

freeVarsRefs :: [Ref] -> IntSet
freeVarsRefs = foldr (\r -> IntSet.union (freeVarsRef r)) IntSet.empty

freeVarsLetExpr :: LetExpr -> IntSet
freeVarsLetExpr (Ap _ v vs) = freeVarsRefs (v:vs)
freeVarsLetExpr (Pap _ vs) = freeVarsRefs vs
freeVarsLetExpr (MkLazy _ _ _) = IntSet.empty
freeVarsLetExpr (Force _ v vs) = freeVarsRefs (v : projForceArgs vs)
freeVarsLetExpr (Pforce _ v vs) = IntSet.insert v (freeVarsRefs vs)
freeVarsLetExpr (CtorBox _ vs) = freeVarsRefs vs
freeVarsLetExpr (Proj _ v) = freeVarsRef v

freeVarsFunExpr :: FunExpr -> IntSet
freeVarsFunExpr (Ret v) = freeVarsRef v
freeVarsFunExpr (VarFieldCnt _ _ e) = freeVarsFunExpr e
freeVarsFunExpr (Let v e1 e2) =
  let e1' = freeVarsLetExpr e1
      e2' = freeVarsFunExpr e2
  in IntSet.difference (IntSet.union e1' e2') (IntSet.singleton v)
freeVarsFunExpr (Case v cs) =
  let cs' = foldl (\a (_, _, x) ->
                    IntSet.union a (freeVarsFunExpr x)) IntSet.empty cs
  in IntSet.union (freeVarsRef v) cs'

freeVarsConstExpr :: ConstExpr -> IntSet
freeVarsConstExpr (Extern _) = IntSet.empty
freeVarsConstExpr (Lazy _ vs e) =
  IntSet.difference (freeVarsFunExpr e) (IntSet.fromList vs)
freeVarsConstExpr (Fun vs e) =
  IntSet.difference (freeVarsFunExpr e) (IntSet.fromList vs)

type Program = IntMap (Const, ConstExpr)

data St = St
  { hlProgram :: Hl.Program
  , program :: Program
  , varMap :: IntMap (Ref, IntSet, IntSet)
                -- Hl.Var -> (Ref, free vars in Ref, consts in Ref)
  , constMap :: IntMap Const
  , constToVarMap :: IntMap Var -- Const -> Var
    -- To subst a (ConstRef _) for something else:
  , constSubst :: IntMap (Either Ref LetExpr)
  , missingSubstitutions :: IntMap (Var, Ref) -- Const -> Substitution
  , constNamePostfix :: String
  , constNameIndex :: Int
  , nextConst :: Int
  , nextVar :: Int
  , optimize :: Bool
  , needsMain :: Bool
  }

type StM = State St

baseIr :: Bool -> Bool -> Hl.Program -> Hl.ProgramRoots -> Program
baseIr doOptimize doNeedMain p r =
  let st = St { hlProgram = p
              , program = IntMap.empty
              , varMap = IntMap.empty
              , constMap = IntMap.empty
              , constToVarMap = IntMap.empty
              , constSubst = IntMap.empty
              , constNamePostfix = ""
              , constNameIndex = 0
              , missingSubstitutions = IntMap.empty
              , nextConst = 0
              , nextVar = 0
              , optimize = doOptimize
              , needsMain = doNeedMain
              }
  in program (execState (irInitProgram r) st)

withPostfix :: String -> StM a -> StM a
withPostfix pfx m = do
  st <- get
  let p0 = constNamePostfix st
  let idx = constNameIndex st
  let ns = st {constNamePostfix = pfx, constNameIndex = 0}
  (x, st') <- lift (runStateT m ns)
  put (st' {constNamePostfix = p0, constNameIndex = idx})
  return x

appendConstNamePostfix :: Bool -> String -> StM String
appendConstNamePostfix b n = do
  st <- get
  if constNamePostfix st == ""
  then return n
  else do
    let i = constNameIndex st
    put (st {constNameIndex = i + 1})
    let n' = n ++ show i
    if b then return (n' ++ Str.funcNameSep ++ constNamePostfix st)
         else return (n' ++ constNamePostfix st)

getNextConstMaybeSep :: Bool -> String -> StM Const
getNextConstMaybeSep b n = do
  st <- get
  let c = nextConst st
  put (st {nextConst = c + 1})
  n' <- appendConstNamePostfix b n
  return (Const n' c)

getNextConst :: String -> StM Const
getNextConst = getNextConstMaybeSep True

getNextFunConst :: StM Const
getNextFunConst = getNextConstMaybeSep False Str.anonymousFunString

getNextVar :: StM Var
getNextVar = do
  st <- get
  let i = nextVar st
  put (st {nextVar = i + 1})
  return i

{-
updateNextVar :: Hl.Var -> StM Var
updateNextVar v = do
  v' <- getNextVar
  updateVarMap v (VarRef v', IntSet.singleton v', IntSet.empty)
  return v'
-}

updateConstMap :: Hl.Const -> Const -> StM ()
updateConstMap c0 c1 = do
  let upd st = IntMap.insert (Hl.prConst c0) c1 (constMap st)
  modify (\st -> st {constMap = upd st})

lookupConstMap :: Hl.Const -> StM Const
lookupConstMap c =
  fmap (fromJust . IntMap.lookup (Hl.prConst c) . constMap) get

updateConstToVarMap :: Const -> Var -> StM ()
updateConstToVarMap c v = do
  let upd st = IntMap.insert (prConst c) v (constToVarMap st)
  modify (\st -> st {constToVarMap = upd st})

tryLookupConstToVarMap :: Const -> StM (Maybe Var)
tryLookupConstToVarMap c =
  fmap (IntMap.lookup (prConst c) . constToVarMap) get

deleteConstToVarMap :: Const -> StM ()
deleteConstToVarMap c = do
  let upd st = IntMap.delete (prConst c) (constToVarMap st)
  modify (\st -> st {constToVarMap = upd st})

forceLookupConstToVarMap :: Const -> StM Var
forceLookupConstToVarMap = fmap fromJust . tryLookupConstToVarMap

updateConstSubst :: Const -> Either Ref LetExpr -> StM ()
updateConstSubst c e = do
  let upd st = IntMap.insert (prConst c) e (constSubst st)
  modify (\st -> st {constSubst = upd st})

tryLookupConstSubst :: Const -> StM (Maybe (Either Ref LetExpr))
tryLookupConstSubst c = do
  fmap (IntMap.lookup (prConst c) . constSubst) get

forceLookupConstSubst :: Const -> StM (Either Ref LetExpr)
forceLookupConstSubst c = do
  x <- tryLookupConstSubst c
  case x of
    Nothing -> return (Left (ConstRef c))
    Just x' -> return x'

updateVarMap :: Hl.Var -> (Ref, IntSet, IntSet) -> StM ()
updateVarMap v0 v1 = do
  let upd st = IntMap.insert v0 v1 (varMap st)
  modify (\st -> st {varMap = upd st})

updateVarMapVar :: Hl.Var -> Var -> StM ()
updateVarMapVar v0 v1 = do
  let upd st = IntMap.insert v0
                  (VarRef v1, IntSet.singleton v1, IntSet.empty) (varMap st)
  modify (\st -> st {varMap = upd st})

lookupVarMap :: Hl.Var -> StM (Ref, IntSet, IntSet)
lookupVarMap v =
  fmap (fromJust . IntMap.lookup v . varMap) get

lookupVarMapRef :: Hl.Var -> StM Ref
lookupVarMapRef v = do
  (x, _, _) <- lookupVarMap v
  return x

updateProgram :: Const -> ConstExpr -> StM ()
updateProgram c e =
  let upd st = IntMap.insert (prConst c) (c, e) (program st)
  in modify (\st -> st {program = upd st})

updateMissingSubstitution :: Const -> (Var, Ref) -> StM ()
updateMissingSubstitution c s =
  let upd st = IntMap.insert (prConst c) s (missingSubstitutions st)
  in modify (\st -> st {missingSubstitutions = upd st})

lookupMissingSubstitution :: Const -> StM (Maybe (Var, Ref))
lookupMissingSubstitution c =
  fmap (IntMap.lookup (prConst c) . missingSubstitutions) get

makeMaps :: Hl.ProgramRoots -> StM ()
makeMaps rs = do
  evalStateT (mapM_ (addToVarMap . Hl.ConstRef) rs) IntSet.empty
  setNextVar
  makeConstMaps rs
  p <- fmap hlProgram get
  cleanupConstToVarMap (IntMap.elems p)
  where
    makeConstMaps :: Hl.ProgramRoots -> StM ()
    makeConstMaps [] = return ()
    makeConstMaps (s:ss) = do
      p <- fmap hlProgram get
      let (e, b) = Hl.lookupConst s p
      d' <- addToConstMaps (s, e, b)
      withPostfix (nameConst d') (doMakeConstMaps e)
      makeConstMaps ss

    setNextVar :: StM ()
    setNextVar = do
      st <- get
      let n = if null (varMap st) then 0 else maximum (IntMap.keys (varMap st))
      put (st {nextVar = n + 1})

    addToVarMap :: Hl.Expr -> StateT IntSet StM ()
    addToVarMap (Hl.Ap _ _ _) = return ()
    addToVarMap (Hl.Force _ _) = return ()
    addToVarMap (Hl.Fun vs e) = do
      mapM_ (\v -> lift (updateVarMapVar v v)) vs
      addToVarMap e
    addToVarMap (Hl.Extern _) = return ()
    addToVarMap (Hl.Lazy _ e) = addToVarMap e
    addToVarMap (Hl.Case _ cs) = do
      mapM_ (\(_, _, c) -> addToVarMap c) cs
    addToVarMap (Hl.ConstRef c) = do
      m <- get
      if IntSet.member (Hl.prConst c) m
      then return ()
      else do
        put (IntSet.insert (Hl.prConst c) m)
        p <- lift (fmap hlProgram get)
        let (e, _) = Hl.lookupConst c p
        addToVarMap e
    addToVarMap (Hl.CtorRef _ _) = return ()
    addToVarMap (Hl.Proj _ _) = return ()
    addToVarMap (Hl.Let v e1 e2) = do
      lift (updateVarMapVar v v)
      addToVarMap e1
      addToVarMap e2
    addToVarMap (Hl.Ret _) = return ()
    addToVarMap (Hl.Where e w) = do
      addToVarMap e
      mapM_ (\c -> addToVarMap (Hl.ConstRef c)) w

doMakeConstMaps :: Hl.Expr -> StM ()
doMakeConstMaps (Hl.Ap _ _ _) = return ()
doMakeConstMaps (Hl.Force _ _) = return ()
doMakeConstMaps (Hl.Fun _ e) = doMakeConstMaps e
doMakeConstMaps (Hl.Extern _) = return ()
doMakeConstMaps (Hl.Lazy _ e) = doMakeConstMaps e
doMakeConstMaps (Hl.Case _ cs) =
  mapM_ (\(_, _, x) -> doMakeConstMaps x) cs
doMakeConstMaps (Hl.ConstRef _) = return ()
doMakeConstMaps (Hl.CtorRef _ _) = return ()
doMakeConstMaps (Hl.Proj _ _) = return ()
doMakeConstMaps (Hl.Let _ e1 e2) = do
  doMakeConstMaps e1
  doMakeConstMaps e2
doMakeConstMaps (Hl.Ret _) = return ()
doMakeConstMaps (Hl.Where e w) = do
  mapM_ makeWithPostfix w
  doMakeConstMaps e
  where
    makeWithPostfix :: Hl.Const -> StM ()
    makeWithPostfix d = do
      p <- fmap hlProgram get
      let (a, b) = Hl.lookupConst d p
      d' <- addToConstMaps (d, a, b)
      withPostfix (nameConst d') (doMakeConstMaps a)

addToConstMaps :: (Hl.Const, Hl.Expr, Bool) -> StM Const
addToConstMaps (con, cone, isStatic) = do
  con' <- getNextConst (Hl.nameConst con)
  updateConstMap con con'
  when (not isStatic && not (isFun cone)) $ do
    v <- getNextVar
    updateConstToVarMap con' v
  return con'
  where
    isFun :: Hl.Expr -> Bool
    isFun (Hl.Fun _ _) = True
    isFun _ = False

cleanupConstToVarMap :: [(Hl.Const, Hl.Expr, Bool)] -> StM ()
cleanupConstToVarMap [] = return ()
cleanupConstToVarMap ((_, _, True) : ss) = cleanupConstToVarMap ss
cleanupConstToVarMap ((s, e, False) : ss) = do
  nv <- nonStaticConstExprNeedsVar e
  when (not nv) $ do
    c <- lookupConstMap s
    deleteConstToVarMap c
  cleanupConstToVarMap ss

nonStaticConstExprNeedsVar :: Hl.Expr -> StM Bool
nonStaticConstExprNeedsVar (Hl.Extern _) = return False
nonStaticConstExprNeedsVar (Hl.Fun _ _) = return False
nonStaticConstExprNeedsVar (Hl.Lazy True _) = return True
nonStaticConstExprNeedsVar e = do
  (fs, _) <- nonStaticConstUnits e
  return (not (IntSet.null fs))

type DeadCodeConsts = IntSet

irOptimizeLoop :: Hl.ProgramRoots -> DeadCodeConsts -> Bool -> StM ()
irOptimizeLoop roots cs hasInlinedCases = do
  b1 <- funInline False
  (b2, cs') <- removeDeadLets cs
  removeUnusedVals roots
  if b1 || b2
    then irOptimizeLoop roots cs' hasInlinedCases
    else return ()

irOptimize :: Hl.ProgramRoots -> StM ()
irOptimize roots = do
  removeOneBranchCases
  irOptimizeLoop roots IntSet.empty False

irInitProgram :: Hl.ProgramRoots -> StM ()
irInitProgram p = do
  makeMaps p
  _ <- irProgram Nothing p
  p' <- fmap program get
  mapM_ updatePostConstSubst (IntMap.elems p')
  p'' <- fmap program get
  mapM_ updatePostMissingSubst (IntMap.elems p'')
  nm <- fmap needsMain get
  when nm (removeUnusedVals p)
  opt <- fmap optimize get
  when opt (irOptimize p)
  where
    updatePostConstSubst :: (Const, ConstExpr) -> StM ()
    updatePostConstSubst (c, e) =
      constSubstConstExpr e >>= updateProgram c

    updatePostMissingSubst :: (Const, ConstExpr) -> StM ()
    updatePostMissingSubst (c, e) = do
      s0 <- lookupMissingSubstitution c
      case s0 of
        Nothing -> return ()
        Just (v, r) ->
          case e of
            (Extern _) -> return ()
            Lazy io vs e' ->
              updateProgram c (Lazy io vs (listSubst [v] [r] e'))
            Fun vs e' ->
              updateProgram c (Fun vs (listSubst [v] [r] e'))

constSubstRefs :: [Ref] -> StM ([Ref], FunExpr -> FunExpr)
constSubstRefs [] = return ([], id)
constSubstRefs (v:vs) = do
  (v', f) <- constSubstRef v
  (vs', g) <- constSubstRefs vs
  return (v' : vs', f . g)

constSubstRef :: Ref -> StM (Ref, FunExpr -> FunExpr)
constSubstRef (ConstRef c) = do
  r <- forceLookupConstSubst c
  case r of
    Left r' -> return (r', id)
    Right e -> do
      v <- getNextVar
      return (VarRef v, Let v e)
constSubstRef (VarRef v) = return (VarRef v, id)

constSubstApExpr :: Var -> Ref -> StM (Ref, FunExpr -> FunExpr)
constSubstApExpr v0 r0@(ConstRef c) = do
  r <- forceLookupConstSubst c
  case r of
    Left (VarRef v') ->
      if v' == v0 then return (r0, id) else return (VarRef v', id)
    Left r' -> return (r', id)
    Right e -> do
      v <- getNextVar
      return (VarRef v, Let v e)
constSubstApExpr _ (VarRef v) = return (VarRef v, id)

constSubstFunExpr :: FunExpr -> StM FunExpr
constSubstFunExpr (Ret v) = do
  (v', f) <- constSubstRef v
  return (f (Ret v'))
constSubstFunExpr (VarFieldCnt v n e) = do
  e' <- constSubstFunExpr e
  return (VarFieldCnt v n e')
constSubstFunExpr (Let v0 (Ap io v vs) e) = do
  (r, f0) <- constSubstApExpr v0 v
  (rs, f1) <- constSubstRefs vs
  e' <- constSubstFunExpr e
  return (f0 (f1 (Let v0 (Ap io r rs) e')))
constSubstFunExpr (Let v0 (MkLazy isForced lp c) e) = do
  e' <- constSubstFunExpr e
  return (Let v0 (MkLazy isForced lp c) e')
constSubstFunExpr (Let v0 (Force io v vs) e) = do
  (r, f0) <- constSubstApExpr v0 v
  (rs, f1) <- case vs of
                Nothing -> return (Nothing, id)
                Just (c, vs') ->
                  fmap (\(x, f) -> (Just (c, x), f)) (constSubstRefs vs')
  e' <- constSubstFunExpr e
  return (f0 (f1 (Let v0 (Force io r rs) e')))
constSubstFunExpr (Let v0 (CtorBox c vs) e) = do
  (vs', f) <- constSubstRefs vs
  e' <- constSubstFunExpr e
  return (f (Let v0 (CtorBox c vs') e'))
constSubstFunExpr (Let v0 (Pap c vs) e) = do
  x <- tryLookupConstSubst c
  let !() = assert (isNothing x) ()
  (vs', f) <- constSubstRefs vs
  e' <- constSubstFunExpr e
  return (f (Let v0 (Pap c vs') e'))
constSubstFunExpr (Let v0 (Pforce pc v vs) e) = do
  (vs', f) <- constSubstRefs vs
  e' <- constSubstFunExpr e
  return (f (Let v0 (Pforce pc v vs') e'))
constSubstFunExpr (Let v0 (Proj i v) e) = do
  (v', f) <- constSubstRef v
  e' <- constSubstFunExpr e
  return (f (Let v0 (Proj i v') e'))
constSubstFunExpr (Case v cs) = do
  cs' <- mapM (\(i, n, c) -> fmap (\x -> (i, n, x)) (constSubstFunExpr c)) cs
  (v', f) <- constSubstRef v
  return (f (Case v' cs'))

constSubstConstExpr :: ConstExpr -> StM ConstExpr
constSubstConstExpr (Extern vs) = return (Extern vs)
constSubstConstExpr (Lazy iss vs e) =
  fmap (Lazy iss vs) (constSubstFunExpr e)
constSubstConstExpr (Fun vs e) =
  fmap (Fun vs) (constSubstFunExpr e)

irProgram :: Maybe Hl.Expr -> Hl.ProgramRoots -> StM (Maybe (FunExpr -> FunExpr))
irProgram parentExpr rs = do
  mapConstMap rs >>= doIr
  where
    mapConstMap :: Hl.ProgramRoots -> StM [(Hl.Const, Const, Hl.Expr)]
    mapConstMap [] = return []
    mapConstMap (c : cs) = do
      c' <- lookupConstMap c
      p <- fmap hlProgram get
      let (e, _) = Hl.lookupConst c p
      cs' <- mapConstMap cs
      return ((c, c', e) : cs')

    doIr :: [(Hl.Const, Const, Hl.Expr)] -> StM (Maybe (FunExpr -> FunExpr))
    doIr [] = return Nothing
    doIr ((hc, c, e) : cs) = do
      (e', f, as) <- withPostfix (nameConst c) (irConstExpr Nothing c e)
      updateProgram c e'
      f' <- doIr cs
      case f of
        Nothing -> return f'
        Just (v, x) -> do
          case parentExpr of
            Nothing -> error "free variables in global scope"
            Just pe -> do
              pg <- fmap hlProgram get
              let u = maxUseCountAtMostOne pg hc pe
              when (not (isLazy e) && u) $
                case e' of
                  Lazy _ vs e'' -> do
                    c' <- withPostfix ""
                            (getNextConst (Str.funFromLazyString ++ nameConst c))
                    updateProgram c' (Fun vs e'')
                    updateConstSubst c (Right (Ap False (ConstRef c') as))
                  _ -> return ()
              -- Need to keep the Let expression, and let optimisation
              -- remove it later, since the variable v may be used elsewhere.
              return (Just (Let v x . maybeFunExprMap f'))

    isLazy :: Hl.Expr -> Bool
    isLazy (Hl.Lazy _ _) = True
    isLazy _ = False

    maxUseCountAtMostOne ::
      Hl.Program -> Hl.Const -> Hl.Expr -> Bool
    maxUseCountAtMostOne p c e =
      maxUseCount IntSet.empty p c (fst (Hl.lookupConst c p)) == Just 0
      && case maxUseCount IntSet.empty p c e of 
          Nothing -> False
          Just n -> n <= 1

    maxUseCount :: IntSet -> Hl.Program -> Hl.Const -> Hl.Expr -> Maybe Int
    maxUseCount _ _ _ (Hl.Ap _ _ _) = Just 0
    maxUseCount _ _ _ (Hl.Force _ _) = Just 0
    maxUseCount vis p c (Hl.Fun _ e) = do
      u <- maxUseCount vis p c e
      if u == 0 then Just 0 else Nothing
    maxUseCount _ _ _ (Hl.Extern _) = Just 0
    maxUseCount vis p c (Hl.Lazy io e) = do
      u <- maxUseCount vis p c e
      if io && u /= 0 then Nothing else Just u
    maxUseCount vis p c (Hl.Case _ cs) =
      fmap (foldl max 0) (mapM (\(_, _, x) -> maxUseCount vis p c x) cs)
    maxUseCount vis p c (Hl.ConstRef c') =
      if Hl.prConst c == Hl.prConst c'
      then Just 1
      else
        if IntSet.member (Hl.prConst c') vis
        then Just 0
        else let (e, isStatic) = Hl.lookupConst c' p
                 vis' = IntSet.insert (Hl.prConst c') vis
             in if isStatic
                then Just 0
                else maxUseCount vis' p c e
    maxUseCount _ _ _ (Hl.CtorRef _ _) = Just 0
    maxUseCount _ _ _ (Hl.Proj _ _) = Just 0
    maxUseCount vis p c (Hl.Let _ e1 e2) = do
      e1' <- maxUseCount vis p c e1
      e2' <- maxUseCount vis p c e2
      Just (e1' + e2')
    maxUseCount _ _ _ (Hl.Ret _) = Just 0
    maxUseCount vis p c (Hl.Where e _) = maxUseCount vis p c e

nonStaticConstUnits :: Hl.Expr -> StM (IntSet, IntSet)
nonStaticConstUnits = \e -> do
  (vs1, cs1) <- getConstUnits IntSet.empty e
  p <- fmap hlProgram get
  let fs = Hl.freeVars p e
  (vs2, cs2) <- unitsFromVarList (IntSet.elems fs)
  return (IntSet.union vs1 vs2, IntSet.union cs1 cs2)
  where
    getConstUnits :: IntSet -> Hl.Expr -> StM (IntSet, IntSet)
    getConstUnits _ (Hl.Ap _ _ __) = return (IntSet.empty, IntSet.empty)
    getConstUnits _ (Hl.Force _ _) = return (IntSet.empty, IntSet.empty)
    getConstUnits vis (Hl.Fun _ e) = getConstUnits vis e
    getConstUnits _ (Hl.Extern _) = return (IntSet.empty, IntSet.empty)
    getConstUnits vis (Hl.Lazy _ e) = getConstUnits vis e
    getConstUnits vis (Hl.Case _ cs) =
      foldlM (\(as0, bs0) (_, _, x) -> do
                (as1, bs1) <- getConstUnits vis x
                return (IntSet.union as0 as1, IntSet.union bs0 bs1)
             ) (IntSet.empty, IntSet.empty) cs
    getConstUnits vis (Hl.ConstRef r) = do
      if IntSet.member (Hl.prConst r) vis
      then return (IntSet.empty, IntSet.empty)
      else do
        let vis' = IntSet.insert (Hl.prConst r) vis
        p <- fmap hlProgram get
        let (e, isStatic) = Hl.lookupConst r p
        if isStatic
        then do
          return (IntSet.empty, IntSet.empty)
        else do
          (s1, s2) <- getConstUnits vis' e
          r' <- lookupConstMap r
          v <- tryLookupConstToVarMap r'
          case v of
            Nothing ->
              return (s1, s2)
            Just v' ->
              return (IntSet.insert v' s1,
                        IntSet.insert (prConst r') s2)
    getConstUnits _ (Hl.CtorRef _ _) = return (IntSet.empty, IntSet.empty)
    getConstUnits _ (Hl.Proj _ _) = return (IntSet.empty, IntSet.empty)
    getConstUnits vis (Hl.Let _ e1 e2) = do
      (a1, b1) <- getConstUnits vis e1
      (a2, b2) <- getConstUnits vis e2
      return (IntSet.union a1 a2, IntSet.union b1 b2)
    getConstUnits _ (Hl.Ret _) = return (IntSet.empty, IntSet.empty)
    getConstUnits vis (Hl.Where e w) = do
      (d1, d2) <- foldlM (\(m1, m2) c -> do
                      c' <- lookupConstMap c
                      v <- tryLookupConstToVarMap c'
                      case v of
                        Nothing ->
                          return (m1, m2)
                        Just v' ->
                          return (IntSet.insert v' m1,
                                    IntSet.insert (prConst c') m2)
                   ) (IntSet.empty, IntSet.empty) w
      (w1, w2) <- foldlM (\(m1, m2) c -> do
                      p <- fmap hlProgram get
                      let (x, isStatic) = Hl.lookupConst c p
                      if isStatic
                      then return (m1, m2)
                      else do
                        (a, b) <- getConstUnits vis x
                        return (IntSet.union m1 a, IntSet.union m2 b)
                   ) (IntSet.empty, IntSet.empty) w
      (e1, e2) <- getConstUnits vis e
      let r1 = IntSet.difference (IntSet.union e1 w1) d1
      let r2 = IntSet.difference (IntSet.union e2 w2) d2
      return (r1, r2)

    unitsFromVarList :: [Hl.Var] -> StM (IntSet, IntSet)
    unitsFromVarList [] = return (IntSet.empty, IntSet.empty)
    unitsFromVarList (x:xs) = do
      (_, a1, a2) <- lookupVarMap x
      (as1, as2) <- unitsFromVarList xs
      return (IntSet.union as1 a1, IntSet.union as2 a2)

lookupProgramConst :: Const -> StM (Const, ConstExpr)
lookupProgramConst c = do
  p <- fmap program get
  return (c, snd (fromJust (IntMap.lookup (prConst c) p)))

maybeFunExprMap :: Maybe (FunExpr -> FunExpr) -> FunExpr -> FunExpr
maybeFunExprMap Nothing = id
maybeFunExprMap (Just f) = f

irConstExpr ::
  Maybe (FunExpr -> FunExpr) -> Const -> Hl.Expr ->
  StM (ConstExpr, Maybe (Var, LetExpr), [Ref])
irConstExpr Nothing _ (Hl.Extern a) = do
  vs <- makeVars a
  return (Extern vs, Nothing, [])
  where
    makeVars :: Int -> StM [Var]
    makeVars 0 = return []
    makeVars i = do
      v <- getNextVar
      vs <- makeVars (i - 1)
      return (v : vs)
irConstExpr (Just _) _ (Hl.Extern _) =
  error "unexpected nested extern value with free variables"
irConstExpr Nothing con fe@(Hl.Fun vs e) = do
  mapM_ (\v -> updateVarMapVar v v) vs
  e' <- irFunExpr e
  (fs0, _) <- nonStaticConstUnits fe
  let fs = IntSet.elems fs0
  let fs' = map VarRef fs
  when (not (null fs))
    (updateConstSubst con (Right (Pap con fs')))
  return (Fun (fs ++ vs) e', Nothing, [])
irConstExpr Nothing con fe@(Hl.Lazy io e) = do
  e' <- irFunExpr e
  (fs0, cs) <- nonStaticConstUnits fe
  let fs = IntSet.elems fs0
  let fs' = map VarRef fs
  if null fs && not io
  then return (Lazy True [] e', Nothing, [])
  else do
    v <- tryLookupConstToVarMap con
    case v of
      Nothing -> do
        let !() = assert (null fs) ()
        return (Lazy True [] e', Nothing, [])
      Just v' -> do
        if null fs
        then do
          updateConstSubst con (Left (VarRef v'))
          return (Lazy False fs e', Just (v', MkLazy False (v', []) con), [])
        else do
          let isRecursive = IntSet.member (prConst con) cs
          if not isRecursive
          then do
            updateConstSubst con (Right (Pforce con v' fs'))
            return (Lazy False fs e', Just (v', MkLazy False (v', fs') con), [])
          else do
            let fs'' = filter (/= VarRef v') fs'
            updateConstSubst con (Right (Pforce con v' fs''))
            v0 <- getNextVar
            updateMissingSubstitution con (v', VarRef v0)
            let e'' = Let v0 (MkLazy False (v0, fs'') con) e'
            return (Lazy False (filter (/= v') fs) e'',
                      Just (v', MkLazy False (v', fs'') con), [])
irConstExpr Nothing con (Hl.Where e p) = do
  f <- irProgram (Just e) p
  irConstExpr f con e
irConstExpr ls con e = do
  e' <- irFunExpr e
  (fs0, cs) <- nonStaticConstUnits e
  let fs = IntSet.elems fs0
  let fs' = map VarRef fs
  if null fs
  then do
    updateConstSubst con (Right (Force False (ConstRef con) Nothing))
    return (Lazy True [] (maybeFunExprMap ls e'), Nothing, [])
  else do
    let isRecursive = IntSet.member (prConst con) cs
    if not isRecursive
    then do
      v <- forceLookupConstToVarMap con
      updateConstSubst con (Right (Force False (VarRef v) (Just (con, fs'))))
      return (Lazy False fs (maybeFunExprMap ls e'),
                Just (v, MkLazy True (v, []) con), fs')
    else do
      v <- forceLookupConstToVarMap con
      let fs'' = filter (/= VarRef v) fs'
      updateConstSubst con (Right (Force False (VarRef v) (Just (con, fs''))))
      v0 <- getNextVar
      updateMissingSubstitution con (v, VarRef v0)
      let e'' = Let v0 (MkLazy True (v0, []) con) e'
      return (Lazy False (filter (/= v) fs) (maybeFunExprMap ls e''),
                Just (v, MkLazy True (v, []) con), [])

irFunExpr :: Hl.Expr -> StM FunExpr
irFunExpr fn@(Hl.Fun _ _) = do
  c <- getNextFunConst
  (f, _, _) <- irConstExpr Nothing c fn
  updateProgram c f
  return (Ret (ConstRef c))
irFunExpr fn@(Hl.Lazy _ _) = do
  c <- getNextFunConst
  nv <- nonStaticConstExprNeedsVar fn
  when nv (getNextVar >>= updateConstToVarMap c)
  (f, p, _) <- irConstExpr Nothing c fn
  updateProgram c f
  case p of
    Nothing -> return (Ret (ConstRef c))
    Just (v, e) -> return (Let v e (Ret (ConstRef c)))
irFunExpr (Hl.Let v e1 e2) = do
  irLetExpr (letCont v (irFunExpr e2)) e1
irFunExpr (Hl.Ap io v vs) = do
  v' <- lookupVarMapRef v
  vs' <- mapM lookupVarMapRef vs
  v0 <- getNextVar
  return (Let v0 (Ap io v' vs') (Ret (VarRef v0)))
irFunExpr (Hl.Force io v) = do
  v' <- lookupVarMapRef v
  v0 <- getNextVar
  return (Let v0 (Force io v' Nothing) (Ret (VarRef v0)))
irFunExpr (Hl.Extern _) = error "unexpected extern in irFunExpr"
irFunExpr (Hl.Case v cs) = do
  v' <- lookupVarMapRef v
  cs' <- mapM irCase cs
  return (Case v' cs')
  where
    irCase ::
      (CtorId, FieldCnt, Hl.Expr) ->
      StM (CtorId, FieldCnt, FunExpr)
    irCase (i, n, e) = do
      e' <- irFunExpr e
      return (i, n, e')
irFunExpr (Hl.ConstRef c) = do
  c' <- lookupConstMap c
  return (Ret (ConstRef c'))
irFunExpr (Hl.CtorRef c vs) = do
  vs' <- mapM lookupVarMapRef vs
  v0 <- getNextVar
  return (Let v0 (CtorBox (Ctor (Hl.prCtor c)) vs') (Ret (VarRef v0)))
irFunExpr (Hl.Proj i v) = do
  v' <- lookupVarMapRef v
  v0 <- getNextVar
  return (Let v0 (Proj i v') (Ret (VarRef v0)))
irFunExpr (Hl.Ret v) = do
  v' <- lookupVarMapRef v
  return (Ret v')
irFunExpr (Hl.Where e p) = do
  f <- irProgram (Just e) p
  e' <- irFunExpr e
  return (maybeFunExprMap f e')

type LetContDom =
  Either (Ref, IntSet, IntSet)
         (Maybe (Var, Const, IntSet, IntSet), LetExpr)

type LetCont = LetContDom -> StM FunExpr

letCont :: Hl.Var -> StM FunExpr -> LetCont
letCont v e (Left r) = updateVarMap v r >> e
letCont v e (Right (Nothing, x)) = do
  updateVarMapVar v v
  fmap (Let v x) e
letCont v e (Right (Just (v0, c, fs, cs), x)) = do
  updateVarMap v (ConstRef c, fs, cs)
  fmap (Let v0 x) e

irLetExpr :: LetCont -> Hl.Expr -> StM FunExpr
irLetExpr f (Hl.Ap io v vs) = do
  v' <- lookupVarMapRef v
  vs' <- mapM lookupVarMapRef vs
  f (Right (Nothing, Ap io v' vs'))
irLetExpr f (Hl.Force io v) = do
  v' <- lookupVarMapRef v
  f (Right (Nothing, Force io v' Nothing))
irLetExpr _ (Hl.Extern _) = error "unexpected extern in irLetExpr"
irLetExpr f (Hl.ConstRef c) = do
  c' <- lookupConstMap c
  p <- fmap hlProgram get
  (fs, cs) <- nonStaticConstUnits (fst (Hl.lookupConst c p))
  f (Left (ConstRef c', fs, cs))
irLetExpr f (Hl.CtorRef i vs) = do
  vs' <- mapM lookupVarMapRef vs
  f (Right (Nothing, CtorBox (Ctor (Hl.prCtor i)) vs'))
irLetExpr f (Hl.Proj i v) = do
  v' <- lookupVarMapRef v
  f (Right (Nothing, Proj i v'))
irLetExpr f (Hl.Case v cs) = do
  v' <- lookupVarMapRef v
  cs' <- mapM (\(i, n, e) -> do e' <- irLetExpr f e
                                return (i, n, e')) cs
  return (Case v' cs')
irLetExpr f (Hl.Let v e1 e2) = do
  irLetExpr (letCont v (irLetExpr f e2)) e1
irLetExpr f (Hl.Ret v) = do
  x <- lookupVarMap v
  f (Left x)
irLetExpr f fn@(Hl.Fun _ _) = do
  c <- getNextFunConst
  (g, _, _) <- irConstExpr Nothing c fn
  updateProgram c g
  (fs, cs) <- nonStaticConstUnits fn
  f (Left (ConstRef c, fs, cs))
irLetExpr f fn@(Hl.Lazy _ _) = do
  c <- getNextFunConst
  nv <- nonStaticConstExprNeedsVar fn
  when nv (getNextVar >>= updateConstToVarMap c)
  (g, p, _) <- irConstExpr Nothing c fn
  updateProgram c g
  (fs, cs) <- nonStaticConstUnits fn
  case p of
    Nothing -> f (Left (ConstRef c, fs, cs))
    Just (v, e) -> f (Right (Just (v, c, fs, cs), e))
irLetExpr f (Hl.Where e p) = do
  g <- irProgram (Just e) p
  fmap (maybeFunExprMap g) (irLetExpr f e)

---------------------- Inlining ------------------------

funInline :: Bool -> StM Bool
funInline inlineCases = do
  p <- fmap program get
  --(b1, p1) <- doExprInline p
  let (b1, p1) = (False, p)
  (b2, p2) <- doFunInline inlineCases p1
  st <- get
  put (st {program = p2})
  return (b1 || b2)

doFunInline :: Bool -> Program -> StM (Bool, Program)
doFunInline inlineCases = \p ->
  foldrM (\e (a, m) ->
            do (b, m') <- funIn m e
               return (a || b, m')
         ) (False, p) (IntMap.elems p)
  where
    funIn :: Program -> (Const, ConstExpr) -> StM (Bool, Program)
    funIn p (c, Fun vs e) = aux p c vs e
    funIn p (c, Lazy _ vs x@(Ret _)) = aux p c vs x
    funIn p (c, Lazy isStatic vs x@(Let _ (CtorBox _ rs) (Ret _))) =
      if not (null rs) && isStatic
        then return (False, p)
        else aux p c vs x
    funIn p (c, Lazy _ vs x@(Let _ (Force _ _ _) (Ret _))) = aux p c vs x
    funIn p _ = return (False, p)

    aux :: Program -> Const -> [Var] -> FunExpr -> StM (Bool, Program)
    aux p c vs e = do
      let ps = IntMap.elems p
      let candidate = isInlineCandidate inlineCases c e
      ps' <- mapM (doFunIn candidate) ps
      return
        (foldl (\(a, q) (b, d, x) ->
                  (a || b,
                   IntMap.insert (prConst d) (d, x) q)
        ) (False, p) ps')
      where
        doFunIn :: Bool -> (Const, ConstExpr) -> StM (Bool, Const, ConstExpr)
        doFunIn cand (d, x) = do
          (b, e') <- funInlineConstExpr cand c vs e x
          return (b, d, e')

isInlineCandidate :: Bool -> Const -> FunExpr -> Bool
isInlineCandidate inlineCases con = isCandidateFunExpr
  where
    isCandidateFunExpr :: FunExpr -> Bool
    isCandidateFunExpr (Case r cs) =
      inlineCases
      && isCandidateRef r
      && and (map (\(_, _, e) -> isCandidateFunExpr e) cs)
    isCandidateFunExpr (Let _ e1 e2) =
      isCandidateLetExpr e1 && isCandidateFunExpr e2
    isCandidateFunExpr (Ret r) = isCandidateRef r
    isCandidateFunExpr (VarFieldCnt _ _ e) = isCandidateFunExpr e

    isCandidateLetExpr :: LetExpr -> Bool
    isCandidateLetExpr (Ap _ r rs) = and (map isCandidateRef (r:rs))
    isCandidateLetExpr (Pap r rs) = and (map isCandidateRef (ConstRef r : rs))
    isCandidateLetExpr (MkLazy _ _ c) = isCandidateRef (ConstRef c)
    isCandidateLetExpr (Force _ r Nothing) = isCandidateRef r
    isCandidateLetExpr (Force _ r (Just (c, rs))) =
      and (map isCandidateRef (r : ConstRef c : rs))
    isCandidateLetExpr (Pforce _ _ rs) = and (map isCandidateRef rs)
    isCandidateLetExpr (CtorBox _ rs) = and (map isCandidateRef rs)
    isCandidateLetExpr (Proj _ r) = isCandidateRef r

    isCandidateRef :: Ref -> Bool
    isCandidateRef (VarRef _) = True
    isCandidateRef (ConstRef c) = prConst c /= prConst con

funInlineConstExpr ::
  Bool -> Const -> [Var] -> FunExpr -> ConstExpr -> StM (Bool, ConstExpr)
funInlineConstExpr _ _ _ _ (Extern vs) = return (False, Extern vs)
funInlineConstExpr cand con vs e (Fun us d) = do
  (b, x) <- funInlineFunExpr IntMap.empty cand con vs e d
  return (b, Fun us x)
funInlineConstExpr cand con vs e (Lazy iss us d) = do
  (b, x) <- funInlineFunExpr IntMap.empty cand con vs e d
  return (b, Lazy iss us x)

type PapMap = IntMap [Ref]

funInlineFunExpr ::
  PapMap -> Bool -> Const -> [Var] -> FunExpr -> FunExpr -> StM (Bool, FunExpr)
funInlineFunExpr pm cand con vs e (Case r cs) = do
  (b1, cs') <- foldrM (\(i, n, c) (b, ds) ->
                         do (b', c') <- funInlineFunExpr pm cand con vs e c
                            return (b || b', (i, n, c') : ds)
                      ) (False, []) cs
  return (b1, Case r cs')
funInlineFunExpr pm cand con vs e (Let v d1 d2) = do
  (b1, pm', f) <- funInlineLetExpr v pm cand con vs e d1
  (b2, d2') <- funInlineFunExpr pm' cand con vs e d2
  return (b1 || b2, f d2')
funInlineFunExpr _ _ _ _ _ (Ret v) = return (False, Ret v)
funInlineFunExpr pm cand con vs e (VarFieldCnt v0 n0 d) = do
  (b, d') <- funInlineFunExpr pm cand con vs e d
  return (b, VarFieldCnt v0 n0 d')

funInlineLetExpr ::
  Var -> PapMap -> Bool -> Const -> [Var] -> FunExpr -> LetExpr ->
  StM (Bool, PapMap, FunExpr -> FunExpr)
funInlineLetExpr v pm _ _ _ _ x@(CtorBox _ _) = return (False, pm, Let v x)
funInlineLetExpr v pm _ _ _ _ x@(Proj _ _) = return (False, pm, Let v x)
funInlineLetExpr v pm _ con _ _ x@(Pap c rs)
  | prConst con /= prConst c = return (False, pm, Let v x)
  | True = do
      let pm' = IntMap.insert v rs pm
      (b, f) <- papInline v c rs
      return (b, pm', f)
funInlineLetExpr v pm _ con _ _ x@(MkLazy _ _ c)
  | prConst con /= prConst c = return (False, pm, Let v x)
  | True = do
      let pm' = IntMap.insert v [] pm
      return (False, pm', Let v x)
funInlineLetExpr v pm _ _ _ _ x@(Pforce _ w rs) =
  case IntMap.lookup w pm of
    Nothing -> return (False, pm, Let v x)
    Just ss -> do
      let pm' = IntMap.insert v (ss ++ rs) pm
      return (False, pm', Let v x)
funInlineLetExpr v pm cand con vs e x@(Ap io (VarRef w) rs) =
  case IntMap.lookup w pm of
    Nothing -> return (False, pm, Let v x)
    Just ss -> do
      if cand
      then do
        let e0 = listSubst vs (ss ++ rs) e
        e1 <- makeFreshVarsFunExpr IntMap.empty e0
        return (True, pm, \d -> updateLeafsFunExpr v d e1)
      else
        return (True, pm, Let v (Ap io (ConstRef con) (ss ++ rs)))
funInlineLetExpr v pm cand con vs e x@(Force io (VarRef w) rs) =
  case IntMap.lookup w pm of
    Nothing -> return (False, pm, Let v x)
    Just ss -> do
      if cand
      then do
        let e0 = listSubst vs (ss ++ projForceArgs rs) e
        e1 <- makeFreshVarsFunExpr IntMap.empty e0
        return (True, pm, \d -> updateLeafsFunExpr v d e1)
      else do
        (_, ce) <- lookupProgramConst con
        case ce of
          Lazy True _ _ -> do
            let !() = assert (null ss && null (projForceArgs rs)) ()
            return (True, pm, Let v (Force io (ConstRef con) Nothing))
          Lazy False _ _ -> return (False, pm, Let v x)
          _ -> error "force of non-lazy term"
funInlineLetExpr v pm cand con vs e x@(Force _ (ConstRef c) rs)
  | not cand || (prConst con /= prConst c) = return (False, pm, Let v x)
  | True = do
      let e0 = listSubst vs (projForceArgs rs) e
      e1 <- makeFreshVarsFunExpr IntMap.empty e0
      return (True, pm, \d -> updateLeafsFunExpr v d e1)
funInlineLetExpr v pm cand con vs e x@(Ap _ (ConstRef c) rs)
  | not cand || (prConst con /= prConst c) = return (False, pm, Let v x)
  | True = do
      let e0 = listSubst vs rs e
      e1 <- makeFreshVarsFunExpr IntMap.empty e0
      return (True, pm, \d -> updateLeafsFunExpr v d e1)

papInline :: Var -> Const -> [Ref] -> StM (Bool, FunExpr -> FunExpr)
papInline letVar con rs = do
  (_, e) <- lookupProgramConst con
  return (doInline e)
  where
    doInline :: ConstExpr -> (Bool, FunExpr -> FunExpr)
    doInline (Fun vs1 (Let v1 (Ap _ (ConstRef c) vs2) (Ret v2)))
      | not (sameRef (VarRef v1) v2) = defaultRet
      | True =
        {- if foo(x1,x2,x3,x4) = bar(x2,x3,x4)
           then foo{a,b,c,d} -> bar{b,c,d}
           and  foo{a,b,c}   -> bar{b,c}
           and  foo{a,b}     -> bar{b}
           and  foo{a}       -> bar
        -}
        let n = length rs
            !() = assert (n <= length vs1) ()
            (h, t) = splitAt n vs1
        in
        case projIfAllVars vs2 of
          Just vs2' ->
            let idx = suffixIndex 0 vs2' t
            in
              if idx == -1
              then defaultRet
              else
                let h' = take idx vs2'
                    m = IntMap.fromList (zip h rs)
                    rs' = map (\x -> fromJust (IntMap.lookup x m)) h'
                in
                  if not (null rs')
                  then (True, Let letVar (Pap c rs'))
                  else (True, listSubst [letVar] [ConstRef c])
          Nothing -> defaultRet
    doInline _ = defaultRet

    defaultRet :: (Bool, FunExpr -> FunExpr)
    defaultRet = (False, Let letVar (Pap con rs))

    suffixIndex :: Int -> [Var] -> [Var] -> Int
    suffixIndex _ xs [] = length xs
    suffixIndex i (x:xs) (y:ys) =
      if y == x
      then if (x:xs) == (y:ys) then i else -1
      else suffixIndex (i+1) xs (y:ys)
    suffixIndex _ [] (_:_) = -1

    projIfAllVars :: [Ref] -> Maybe [Var]
    projIfAllVars [] = Just []
    projIfAllVars (VarRef v : vs) = fmap (v:) (projIfAllVars vs)
    projIfAllVars (_:_) = Nothing

listSubst :: [Var] -> [Ref] -> FunExpr -> FunExpr
listSubst vs rs =
  let !() = assert (length vs == length rs) ()
  in substFunExpr (IntMap.fromList (zip vs rs))

substFunExpr :: IntMap Ref -> FunExpr -> FunExpr
substFunExpr m (Case r cs) =
  let r' = substRef m r
      cs' = map (\(i, n, c) -> (i, n, substFunExpr m c)) cs
  in Case r' cs'
substFunExpr m (Let v e1 e2) =
  let e1' = substLetExpr m e1
      e2' = substFunExpr (IntMap.delete v m) e2
  in Let v e1' e2'
substFunExpr m (Ret r) = Ret (substRef m r)
substFunExpr m (VarFieldCnt v n e) =
  case IntMap.lookup v m of
    Nothing -> VarFieldCnt v n (substFunExpr m e)
    Just (VarRef v') -> VarFieldCnt v' n (substFunExpr m e)
    Just _ -> substFunExpr m e

substLetExpr :: IntMap Ref -> LetExpr -> LetExpr
substLetExpr m (Ap io r rs) = Ap io (substRef m r) (map (substRef m) rs)
substLetExpr m (Pap c rs) = Pap c (map (substRef m) rs)
substLetExpr _ (MkLazy isForced lp c) = MkLazy isForced lp c
substLetExpr m (Force io r Nothing) = Force io (substRef m r) Nothing
substLetExpr m (Force io r (Just (c, rs))) =
  Force io (substRef m r) (Just (c, map (substRef m) rs))
substLetExpr m (Pforce pc v rs) = 
  case substRef m (VarRef v) of
    VarRef v' -> Pforce pc v' (map (substRef m) rs)
    _ -> error "unexpected substitutuon of Pforce function value"
substLetExpr m (CtorBox i rs) = CtorBox i (map (substRef m) rs)
substLetExpr m (Proj i r) = Proj i (substRef m r)

substRef :: IntMap Ref -> Ref -> Ref
substRef m (VarRef v) =
  case IntMap.lookup v m of
    Nothing -> VarRef v
    Just r -> r
substRef _ r = r

updateLeafsFunExpr :: Var -> FunExpr -> FunExpr -> FunExpr
updateLeafsFunExpr v leaf (Ret r) = listSubst [v] [r] leaf
updateLeafsFunExpr v leaf (VarFieldCnt v0 n0 e) =
  VarFieldCnt v0 n0 (updateLeafsFunExpr v leaf e)
updateLeafsFunExpr v leaf (Case r cs) =
  let cs' = map (\(i, n, c) -> (i, n, updateLeafsFunExpr v leaf c)) cs
  in Case r cs'
updateLeafsFunExpr v leaf (Let w e1 e2) =
  Let w e1 (updateLeafsFunExpr v leaf e2)

type FreshVarMap = IntMap Var

makeFreshVarsConstExpr :: ConstExpr -> StM ConstExpr
makeFreshVarsConstExpr (Extern vs) = do
  vs' <- mapM (const getNextVar) vs
  return (Extern vs')
makeFreshVarsConstExpr (Fun vs e) = do
  vs' <- mapM (const getNextVar) vs
  let m = IntMap.fromList (zip vs vs')
  e' <- makeFreshVarsFunExpr m e
  return (Fun vs' e')
makeFreshVarsConstExpr (Lazy iss vs e) = do
  vs' <- mapM (const getNextVar) vs
  let m = IntMap.fromList (zip vs vs')
  e' <- makeFreshVarsFunExpr m e
  return (Lazy iss vs' e')

makeFreshVarsFunExpr :: FreshVarMap -> FunExpr -> StM FunExpr
makeFreshVarsFunExpr m (Case r cs) = do
  r' <- makeFreshVarsRef m r
  cs' <- mapM (\(i, n, c) -> fmap (\x -> (i, n, x)) (makeFreshVarsFunExpr m c)) cs
  return (Case r' cs')
makeFreshVarsFunExpr m (Let v e1 e2) = do
  v' <- getNextVar
  e1' <- makeFreshVarsLetExpr m e1
  e2' <- makeFreshVarsFunExpr (IntMap.insert v v' m) e2
  return (Let v' e1' e2')
makeFreshVarsFunExpr m (Ret r) = do
  r' <- makeFreshVarsRef m r
  return (Ret r')
makeFreshVarsFunExpr m (VarFieldCnt v n e) = do
  e' <- makeFreshVarsFunExpr m e
  case IntMap.lookup v m of
    Nothing -> return (VarFieldCnt v n e')
    Just v' -> return (VarFieldCnt v' n e')

makeFreshVarsLetExpr :: FreshVarMap -> LetExpr -> StM LetExpr
makeFreshVarsLetExpr m (Ap io r rs) = do
  r' <- makeFreshVarsRef m r
  rs' <- mapM (makeFreshVarsRef m) rs
  return (Ap io r' rs')
makeFreshVarsLetExpr m (Pap c rs) = do
  rs' <- mapM (makeFreshVarsRef m) rs
  return (Pap c rs')
makeFreshVarsLetExpr _ (MkLazy isForced lp c) = return (MkLazy isForced lp c)
makeFreshVarsLetExpr m (Force io r rs) = do
  r' <- makeFreshVarsRef m r
  rs' <- case rs of
          Nothing -> return Nothing
          Just (c, xs) ->
            fmap (\xs' -> Just (c, xs')) (mapM (makeFreshVarsRef m) xs)
  return (Force io r' rs')
makeFreshVarsLetExpr m (Pforce pc v rs) = do
  v' <- makeFreshVarsVar m v
  rs' <- mapM (makeFreshVarsRef m) rs
  return (Pforce pc v' rs')
makeFreshVarsLetExpr m (CtorBox i rs) = do
  rs' <- mapM (makeFreshVarsRef m) rs
  return (CtorBox i rs')
makeFreshVarsLetExpr m (Proj i r) = do
  r' <- makeFreshVarsRef m r
  return (Proj i r')

makeFreshVarsVar :: FreshVarMap -> Var -> StM Var
makeFreshVarsVar m v = 
  case IntMap.lookup v m of
    Nothing -> return v
    Just v' -> return v'

makeFreshVarsRef :: FreshVarMap -> Ref -> StM Ref
makeFreshVarsRef m (VarRef v) = fmap VarRef (makeFreshVarsVar m v)
makeFreshVarsRef _ r = return r

---------------------- Dead code removal ------------------------

removeOneBranchCases :: StM ()
removeOneBranchCases = do
  p <- fmap program get
  aux (IntMap.elems p)
  where
    aux :: [(Const, ConstExpr)] -> StM ()
    aux [] = return ()
    aux ((c, e) : cs) = do
      let e' = removeOneBranchCaseConstExpr e
      updateProgram c e'
      aux cs

removeOneBranchCaseConstExpr :: ConstExpr -> ConstExpr
removeOneBranchCaseConstExpr (Extern vs) = Extern vs
removeOneBranchCaseConstExpr (Fun vs e) =
  Fun vs (removeOneBranchCaseFunExpr e)
removeOneBranchCaseConstExpr (Lazy iss vs e) =
  Lazy iss vs (removeOneBranchCaseFunExpr e)

removeOneBranchCaseFunExpr :: FunExpr -> FunExpr
removeOneBranchCaseFunExpr (Case r cs) =
  let cs' = map (\(i, n, c) -> (i, n, removeOneBranchCaseFunExpr c)) cs
  in case cs' of
      [(_, n, x)] ->
        case r of
          VarRef r' -> VarFieldCnt r' n x
          _ -> x
      _ -> Case r cs'
removeOneBranchCaseFunExpr (Let v e1 e2) = 
  Let v e1 (removeOneBranchCaseFunExpr e2)
removeOneBranchCaseFunExpr (Ret v) = Ret v
removeOneBranchCaseFunExpr (VarFieldCnt v n e) =
  VarFieldCnt v n (removeOneBranchCaseFunExpr e)

removeUnusedVals :: Hl.ProgramRoots -> StM ()
removeUnusedVals roots = do
  (mc, ma) <- findMain
  p <- evalStateT (getReachableConstExpr ma)
        (IntSet.singleton (prConst mc))
  st <- get
  put (st {program = IntMap.insert (prConst mc) (mc, ma) p})
  where
    findMain :: StM (Const, ConstExpr)
    findMain = do
      let x = foldl (\a r -> if Hl.nameConst r == "main"
                              then Just r
                              else a) Nothing roots
      case x of
        Nothing -> error "unable to find main function"
        Just r -> do
          c <- lookupConstMap r
          lookupProgramConst c

getReachableConstExpr :: ConstExpr -> StateT IntSet StM Program
getReachableConstExpr (Extern _) = return IntMap.empty
getReachableConstExpr (Fun _ e) = getReachableFunExpr e
getReachableConstExpr (Lazy _ _ e) = getReachableFunExpr e

getReachableFunExpr :: FunExpr -> StateT IntSet StM Program
getReachableFunExpr (Case _ cs) =
  foldlM (\p (_, _, c) ->
            fmap (IntMap.union p) (getReachableFunExpr c)
         ) IntMap.empty cs
getReachableFunExpr (Let _ e1 e2) = do
  p1 <- getReachableLetExpr e1
  p2 <- getReachableFunExpr e2
  return (IntMap.union p1 p2)
getReachableFunExpr (Ret r) = getReachableRef r
getReachableFunExpr (VarFieldCnt _ _ e) = getReachableFunExpr e

getReachableLetExpr :: LetExpr -> StateT IntSet StM Program
getReachableLetExpr (Ap _ v vs) = getReachableRefs (v:vs)
getReachableLetExpr (Pap c vs) = do
  p1 <- getReachableConst c
  p2 <- getReachableRefs vs
  return (IntMap.union p1 p2)
getReachableLetExpr (MkLazy _ _ c) = getReachableConst c
getReachableLetExpr (Force _ v Nothing) = getReachableRef v
getReachableLetExpr (Force _ v (Just (c, vs))) =
  getReachableRefs (v : ConstRef c : vs)
getReachableLetExpr (Pforce _ _ vs) = getReachableRefs vs
getReachableLetExpr (CtorBox _ vs) = getReachableRefs vs
getReachableLetExpr (Proj _ v) = getReachableRef v

getReachableConst :: Const -> StateT IntSet StM Program
getReachableConst c = do
  vis <- get
  if IntSet.member (prConst c) vis
    then return IntMap.empty
    else do
      put (IntSet.insert (prConst c) vis)
      (_, e) <- lift (lookupProgramConst c)
      p <- getReachableConstExpr e
      return (IntMap.insert (prConst c) (c, e) p)

getReachableRef :: Ref -> StateT IntSet StM Program
getReachableRef (ConstRef c) = getReachableConst c
getReachableRef _ = return IntMap.empty

getReachableRefs :: [Ref] -> StateT IntSet StM Program
getReachableRefs rs = 
  foldlM (\p r ->
            fmap (IntMap.union p) (getReachableRef r)
         ) IntMap.empty rs

removeDeadLets :: DeadCodeConsts -> StM (Bool, DeadCodeConsts)
removeDeadLets dead = do
  p <- fmap program get
  (b, ps, dead') <- doRemoveReadCode (IntMap.elems p)
  mapM_ (\(c,e) -> updateProgram c e) ps
  return (b, dead')
  where
    doRemoveReadCode ::
      [(Const, ConstExpr)] ->
      StM (Bool, [(Const, ConstExpr)], DeadCodeConsts)
    doRemoveReadCode [] = return (False, [], IntSet.empty)
    doRemoveReadCode ((c, e) : cs) = do
      (b1, e', dead1) <- removeDeadCodeConstExpr dead c e
      (b2, cs', dead2) <- doRemoveReadCode cs
      return (b1 || b2, (c, e') : cs', IntSet.union dead1 dead2)

type LiveSet = IntSet
type VarContext = IntSet

removeDeadCodeConstExpr ::
  DeadCodeConsts -> Const -> ConstExpr ->
  StM (Bool, ConstExpr, DeadCodeConsts)
removeDeadCodeConstExpr dead _ (Extern vs) =
  return (False, Extern vs, dead)
removeDeadCodeConstExpr dead c (Fun vs e) = do
  let g = IntSet.fromList vs
  (b1, li, e') <- removeDeadCodeFunExpr c vs g e
  let vs' = filter (flip IntSet.member li) vs
  if length vs == length vs'
    then return (b1, Fun vs e', dead)
    else
      if IntSet.member (prConst c) dead
      then return (b1, Fun vs e', dead)
      else do
        c' <- getNextConst (Str.anonymousDuplicateString
                            ++ nameConst c)
        let dead' = IntSet.insert (prConst c) dead
        f <- makeFreshVarsConstExpr (Fun vs' e')
        updateProgram c' f
        v0 <- getNextVar
        let a = Ap False (ConstRef c') (map VarRef vs')
        let a' = Let v0 a (Ret (VarRef v0))
        return (True, Fun vs a', dead')
removeDeadCodeConstExpr dead c (Lazy iss vs e) = do
  let g = IntSet.fromList vs
  (b, _li, e') <- removeDeadCodeFunExpr c vs g e
  return (b, Lazy iss vs e', dead)
{-
  let vs' = filter (flip IntSet.member li) vs
  if length vs == length vs'
    then return (b, Lazy iss vs e', dead)
    else
      if IntSet.member (prConst c) dead
      then return (b, Lazy iss vs e', dead)
      else do
        c' <- getNextConst (Str.anonymousDuplicateString
                            ++ nameConst c)
        let dead' = IntSet.insert (prConst c) dead
        f <- makeFreshVarsConstExpr (Lazy iss vs' e')
        updateProgram c' f
        v0 <- getNextVar
        let a = Force False (ConstRef c') (map VarRef vs')
        let a' = Let v0 a (Ret (VarRef v0))
        return (True, Lazy iss vs a', dead')
-}

removeDeadCodeFunExpr ::
  Const -> [Var] -> VarContext -> FunExpr -> StM (Bool, LiveSet, FunExpr)
removeDeadCodeFunExpr con params g (Case r cs) = do
  (b2, li2, cs') <- foldrM (\(i, n, c) (a, li, cs') ->
                             do (b, li', c') <- removeDeadCodeFunExpr con params g c
                                return (a || b, IntSet.union li li', (i, n, c') : cs')
                           ) (False, IntSet.empty, []) cs
  (li3, r') <- removeDeadCodeRef r
  let ca = Case r' cs'
  return (b2, IntSet.union li2 li3, ca)
removeDeadCodeFunExpr con params g (Let v e1 e2) = do
  (li1, e1', io) <- removeDeadCodeLetExpr con params e1
  (b1, li2, e2') <- removeDeadCodeFunExpr con params (IntSet.insert v g) e2
  if io || IntSet.member v li2
  then return (b1, IntSet.union li1 li2, Let v e1' e2')
  else return (True, li2, e2')
removeDeadCodeFunExpr _ _ _ (Ret r) = do
  (li, r') <- removeDeadCodeRef r
  return (False, li, Ret r')
removeDeadCodeFunExpr con params g (VarFieldCnt v n e) = do
  (b, li, e') <- removeDeadCodeFunExpr con params g e
  return $
    if IntSet.member v g
    then (b, li, VarFieldCnt v n e')
    else (b, li, e') -- This does not count as removing code / progress,
                     -- so do not blindly return True here.

removeDeadCodeLetExpr ::
  Const -> [Var] -> LetExpr -> StM (LiveSet, LetExpr, Bool)
removeDeadCodeLetExpr con params (Ap io r rs) = do
  (li1, r') <- removeDeadCodeRef r
  (li2, rs') <- removeDeadCodeRefs rs
  let li = tryRemoveArgsIfSameRef con params r' rs' (IntSet.union li1 li2)
  return (li, Ap io r' rs', io)
removeDeadCodeLetExpr con params (Pap c rs) = do
  (li2, rs') <- removeDeadCodeRefs rs
  let li = tryRemoveArgsIfSameRef con params (ConstRef c) rs' li2
  return (li, Pap c rs', False)
removeDeadCodeLetExpr _ _ (MkLazy isForced lp c) =
  return (IntSet.empty, MkLazy isForced lp c, False)
removeDeadCodeLetExpr con params (Force io r rs) = do
  (li1, r') <- removeDeadCodeRef r
  (li2, rs') <- case rs of
                  Nothing -> return (IntSet.empty, Nothing)
                  Just (c, xs) -> do
                    (li, xs') <- removeDeadCodeRefs xs
                    return (li, Just (c, xs'))
  let xs = projForceArgs rs'
  let li = tryRemoveArgsIfSameRef con params r' xs (IntSet.union li1 li2)
  return (li, Force io r' rs', io)
removeDeadCodeLetExpr _ _ (Pforce pc v rs) = do
  (li1, v') <- removeDeadCodeVar v
  (li2, rs') <- removeDeadCodeRefs rs
  return (IntSet.union li1 li2, Pforce pc v' rs', False)
removeDeadCodeLetExpr _ _ (CtorBox i rs) = do
  (li2, rs') <- removeDeadCodeRefs rs
  return (li2, CtorBox i rs', False)
removeDeadCodeLetExpr _ _ (Proj i r) = do
  (li, r') <- removeDeadCodeRef r
  return (li, Proj i r', False)

tryRemoveArgsIfSameRef :: Const -> [Var] -> Ref -> [Ref] -> IntSet -> IntSet
tryRemoveArgsIfSameRef con params (ConstRef con') rs s
  | prConst con == prConst con' =
    let rm = argsToRemove params rs
        ic = argsToInclude params rs
    in IntSet.difference s (IntSet.difference rm ic)
  | True = s
  where
    argsToRemove :: [Var] -> [Ref] -> IntSet
    argsToRemove [] _ = IntSet.empty
    argsToRemove _ [] = IntSet.empty
    argsToRemove (v1 : vs1) (VarRef v2 : vs2) =
      let x = argsToRemove vs1 vs2
      in if v1 == v2
          then IntSet.insert v2 x
          else x
    argsToRemove (_ : vs1) (_ : vs2) = argsToRemove vs1 vs2

    argsToInclude :: [Var] -> [Ref] -> IntSet
    argsToInclude _ [] = IntSet.empty
    argsToInclude [] vs = varSetFromRefs vs
    argsToInclude (v1 : vs1) (VarRef v2 : vs2) =
      let x = argsToInclude vs1 vs2
      in if v1 /= v2
          then IntSet.insert v2 x
          else x
    argsToInclude (_ : vs1) (_ : vs2) = argsToInclude vs1 vs2

    varSetFromRefs :: [Ref] -> IntSet
    varSetFromRefs [] = IntSet.empty
    varSetFromRefs (VarRef v : vs) = IntSet.insert v (varSetFromRefs vs)
    varSetFromRefs (_ : vs) = varSetFromRefs vs
tryRemoveArgsIfSameRef _ _ _ _ s = s

removeDeadCodeVar :: Var -> StM (LiveSet, Var)
removeDeadCodeVar v = return (IntSet.singleton v, v)

removeDeadCodeRef :: Ref -> StM (LiveSet, Ref)
removeDeadCodeRef (VarRef v) = do
  (s, v') <- removeDeadCodeVar v
  return (s, VarRef v')
removeDeadCodeRef r = return (IntSet.empty, r)

removeDeadCodeRefs :: [Ref] -> StM (LiveSet, [Ref])
removeDeadCodeRefs [] = return (IntSet.empty, [])
removeDeadCodeRefs (r:rs) = do
  (li1, r') <- removeDeadCodeRef r
  (li2, rs') <- removeDeadCodeRefs rs
  return (IntSet.union li1 li2, r' : rs')

------------------- Printing -----------------------------

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
writeFunExpr b (VarFieldCnt v n e) = do
  when b (incIndent >> newLine)
  writeStr "size_"
  writeStr (show n)
  writeStr "("
  writeVar v
  writeStr ");"
  newLine
  writeFunExpr False e
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
    writeCases :: [(CtorId, FieldCnt, FunExpr)] -> ToString ()
    writeCases [] = return ()
    writeCases ((i, n, w) : ws) = do
      newLine
      writeStr "of "
      writeStr (show i)
      writeStr "("
      writeStr (show n)
      writeStr ")"
      writeStr " => "
      writeFunExpr True w
      writeCases ws

writeLetExpr :: LetExpr -> ToString ()
writeLetExpr (Ap _ v vs) = writeRef v >> writeParenRefs vs
writeLetExpr (Pap c vs) = writeStr (nameConst c) >> writeCurlyRefs vs
writeLetExpr (MkLazy _ _ c) =
  writeStr "new-lazy(" >> writeStr (nameConst c) >> writeStr ")"
writeLetExpr (Force _ v Nothing) = writeRef v >> writeSquareRefs []
writeLetExpr (Force _ v (Just (_, vs))) = writeRef v >> writeSquareRefs vs
writeLetExpr (Pforce _ v vs) =
  writeVar v >> writeStr "[{" >> writeRefs vs >> writeStr "}]"
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
