module TypeCheck.Env
  ( VarStatus (..)
  , isStatusUnknown
  , isStatusInProgress
  , isStatusTerm
  , EnvT
  , getRefMap
  , getImplicitMap
  , runEnvT
  , TypeCheck.Env.lookup
  , member
  , forceInsert
  , tryInsert
  , tryUpdateIf
  , tryUpdate
  , isInThisScope
  , scope
  , freshVarId
  , freshRefId
  , forceInsertRef
  , forceInsertExtern
  , forceInsertDataCtor
  , forceLookupRefMap
  , forceLookupImplicit
  , isDataType
  , forceLookupDataCtor
  , forceInsertImplicit
  , forceLookupImplicitMap
  , forceLookupDataCtorMap
  , forceInsertRefMeta
  , forceLookupRefMeta
  , lookupRef
  , isExtern
  , forceLookupRef
  , memberRef
  , toString
  , withDepth
  , getDepth
  , getNextVarId
  , ImplicitVarMap
  , getImplicitVarMap
  , insertImplicitVarCurrentScope
  , clearImplicitVarMap
  )
where

import Data.Maybe (isJust)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State
import ParseTree
import TypeCheck.Term
import Loc (Loc)
import Str (quote)
--import Debug.Trace (trace)

type EnvDepth = Int

type ScopeId = Int

data VarStatus = StatusTerm Term
               | StatusUnknownCtor SubstMap EnvDepth (Loc, VarName) Decl
               | StatusUnknownRef SubstMap EnvDepth Def
               | StatusUnknownVar SubstMap EnvDepth (Loc, VarName) VarId (Maybe Expr)
               | StatusInProgress Bool (Loc, VarName) -- Bool true is ctor
               deriving Show

type ScopeMap = Map.Map VarName VarStatus

data Env = Env ScopeId ScopeMap (Maybe Env) deriving Show

type ImplicitVarMap = IntMap.IntMap (PreTerm, [ScopeId])

data EnvSt = EnvSt
  { env :: Env
  , nextVarId :: VarId
  , nextRefId :: VarId
  , currentScopeId :: ScopeId
  , refMap :: RefMap
  , implicitVarMap :: ImplicitVarMap
  , externSet :: ExternSet
  , implicitMap :: ImplicitMap
  , dataCtorMap :: DataCtorMap }
  deriving Show

type EnvT = StateT EnvSt

isStatusUnknown :: VarStatus -> Bool
isStatusUnknown (StatusUnknownCtor _ _ _ _) = True
isStatusUnknown (StatusUnknownRef _ _ _) = True
isStatusUnknown (StatusUnknownVar _ _ _ _ _) = True
isStatusUnknown _ = False

isStatusInProgress :: VarStatus -> Bool
isStatusInProgress (StatusInProgress _ _) = True
isStatusInProgress _ = False

isStatusTerm :: VarStatus -> Bool
isStatusTerm (StatusTerm _) = True
isStatusTerm _ = False

emptyCtxSt :: EnvSt
emptyCtxSt =
  EnvSt
    { env = Env 0 Map.empty Nothing
    , nextVarId = 0
    , nextRefId = 0
    , currentScopeId = 0
    , refMap = IntMap.empty
    , implicitVarMap = IntMap.empty
    , externSet = IntSet.empty
    , implicitMap = IntMap.empty
    , dataCtorMap = IntMap.empty }

toString :: Monad m => EnvT m String
toString = fmap show getEnv

runEnvT :: Monad m => EnvT m a -> m (a, DataCtorMap, ImplicitMap, RefMap)
runEnvT s =
  fmap (\(x,r) -> (x, dataCtorMap r, implicitMap r, refMap r)) (runStateT s emptyCtxSt)

getCurrentScopeId :: Monad m => EnvT m ScopeId
getCurrentScopeId = fmap currentScopeId get

incCurrentScopeId :: Monad m => EnvT m ()
incCurrentScopeId = modify (\s -> s{currentScopeId = currentScopeId s + 1})

getRefMap :: Monad m => EnvT m RefMap
getRefMap = fmap refMap get

getImplicitVarMap :: Monad m => EnvT m ImplicitVarMap
getImplicitVarMap = fmap implicitVarMap get

getExternSet :: Monad m => EnvT m ExternSet
getExternSet = fmap externSet get

getImplicitMap :: Monad m => EnvT m ImplicitMap
getImplicitMap = fmap implicitMap get

getDataCtorMap :: Monad m => EnvT m DataCtorMap
getDataCtorMap = fmap dataCtorMap get

modifyRefMap :: Monad m => (RefMap -> RefMap) -> EnvT m ()
modifyRefMap f = modify (\s -> s{refMap = f (refMap s)})

modifyImplicitVarMap :: Monad m => (ImplicitVarMap -> ImplicitVarMap) -> EnvT m ()
modifyImplicitVarMap f = modify (\s -> s{implicitVarMap = f (implicitVarMap s)})

modifyExternSet :: Monad m => (ExternSet -> ExternSet) -> EnvT m ()
modifyExternSet f = modify (\s -> s{externSet = f (externSet s)})

modifyImplicitMap :: Monad m => (ImplicitMap -> ImplicitMap) -> EnvT m ()
modifyImplicitMap f = modify (\s -> s{implicitMap = f (implicitMap s)})

modifyDataCtorMap :: Monad m => (DataCtorMap -> DataCtorMap) -> EnvT m ()
modifyDataCtorMap f = modify (\s -> s{dataCtorMap = f (dataCtorMap s)})

getNextVarId :: Monad m => EnvT m VarId
getNextVarId = fmap nextVarId get

modifyNextVarId :: Monad m => (VarId -> VarId) -> EnvT m ()
modifyNextVarId f = modify (\s -> s{nextVarId = f (nextVarId s)})

_putNextVarId :: Monad m => VarId -> EnvT m ()
_putNextVarId i = modifyNextVarId (const i)

updateNextVarId :: Monad m => (VarId -> VarId) -> EnvT m VarId
updateNextVarId f = do
  i <- getNextVarId
  modifyNextVarId f
  return i

getNextRefId :: Monad m => EnvT m VarId
getNextRefId = fmap nextRefId get

modifyNextRefId :: Monad m => (VarId -> VarId) -> EnvT m ()
modifyNextRefId f = modify (\s -> s{nextRefId = f (nextRefId s)})

_putNextRefId :: Monad m => VarId -> EnvT m ()
_putNextRefId i = modifyNextRefId (const i)

updateNextRefId :: Monad m => (VarId -> VarId) -> EnvT m VarId
updateNextRefId f = do
  i <- getNextRefId
  modifyNextRefId f
  return i

getEnv :: Monad m => EnvT m Env
getEnv = fmap env get

modifyEnv :: Monad m => (Env -> Env) -> EnvT m ()
modifyEnv f = modify (\s -> s{env = f (env s)})

putEnv :: Monad m => Env -> EnvT m ()
putEnv s = modifyEnv (const s)

doLookup :: VarName -> Env -> Maybe VarStatus
doLookup v (Env _ m Nothing) = Map.lookup v m
doLookup v (Env _ m (Just p)) =
  case Map.lookup v m of
    Nothing -> doLookup v p
    Just e -> Just e

lookup :: Monad m => VarName -> EnvT m (Maybe VarStatus)
lookup n = fmap (doLookup n) getEnv

member :: Monad m => VarName -> EnvT m Bool
member = fmap isJust . TypeCheck.Env.lookup

doForceInsert :: VarName -> VarStatus -> Env -> Env
doForceInsert n x (Env sid m p) = Env sid (Map.insert n x m) p

forceInsert :: Monad m => VarName -> VarStatus -> EnvT m ()
forceInsert n p = modifyEnv (doForceInsert n p)

doTryInsert :: VarName -> VarStatus -> Env -> Either VarStatus Env
doTryInsert n x (Env sid m p) =
  case Map.lookup n m of
    Nothing -> Right (Env sid (Map.insert n x m) p)
    Just s -> Left s

tryInsert ::
  Monad m => VarName -> VarStatus -> EnvT m (Maybe VarStatus)
tryInsert n x = do
  a <- fmap (doTryInsert n x) getEnv
  case a of
    Right c -> putEnv c >> return Nothing
    Left s -> return (Just s)

isInThisScope :: Monad m => VarName -> EnvT m Bool
isInThisScope n = do
  Env _ m _ <- getEnv
  return (Map.member n m)

doUpdateIf ::
  (VarStatus -> Bool) -> VarName -> VarStatus -> Env ->
  Maybe (Env, VarStatus)
doUpdateIf p n x (Env sid m c) =
  case Map.lookup n m of
    Nothing -> do
      (c', r) <- c >>= doUpdateIf p n x
      return (Env sid m (Just c'), r)
    Just s -> if p s
                then Just (Env sid (Map.insert n x m) c, s)
                else Nothing

tryUpdateIf :: Monad m =>
  (VarStatus -> Bool) -> VarName -> VarStatus -> EnvT m (Maybe VarStatus)
tryUpdateIf p n x = do
  a <- fmap (doUpdateIf p n x) getEnv
  case a of
    Nothing -> return Nothing
    Just (c, s) -> putEnv c >> return (Just s)

tryUpdate ::
  Monad m => VarName -> VarStatus -> EnvT m (Maybe VarStatus)
tryUpdate = tryUpdateIf (const True)

envList :: Env -> [(ScopeId, ScopeMap)]
envList = accEnvList []
  where
    accEnvList :: [(ScopeId, ScopeMap)] -> Env -> [(ScopeId, ScopeMap)]
    accEnvList acc (Env sid m Nothing) = (sid, m) : acc
    accEnvList acc (Env sid m (Just c)) = accEnvList ((sid, m) : acc) c

envTrace :: Env -> [ScopeId]
envTrace (Env sid _ Nothing) = [sid]
envTrace (Env sid _ (Just c)) = sid : envTrace c

getDepth :: Monad m => EnvT m EnvDepth
getDepth = fmap envDepth getEnv
  where envDepth :: Env -> EnvDepth
        envDepth (Env _ _ Nothing) = 0
        envDepth (Env _ _ (Just c)) = 1 + envDepth c

withDepth :: Monad m => EnvDepth -> EnvT m a -> EnvT m a
withDepth i c = do
  s <- get
  let e = snd (capEnv (env s))
  (x, s') <- lift (runStateT c (s {env = e}))
  let (m2 : t2) = envList (env s')
  put s'
  putEnv (mapEnv (Env (fst m2) (snd m2) Nothing) (tail (envList (env s))) t2)
  return x
  where capEnv :: Env -> (EnvDepth, Env)
        capEnv (Env sid m Nothing) = (0, Env sid m Nothing)
        capEnv (Env sid m (Just e)) =
          let (n, e') = capEnv e
          in if n >= i then (n, e') else (n + 1, Env sid m (Just e))

        mapEnv :: Env -> [(ScopeId, ScopeMap)] -> [(ScopeId, ScopeMap)] -> Env
        mapEnv e (_ : xs) ((k, y) : ys) = mapEnv (Env k y (Just e)) xs ys
        mapEnv e ((j, x) : xs) [] = mapEnv (Env j x (Just e)) xs []
        mapEnv e [] [] = e
        mapEnv _ [] (_ : _) = error "unreachable case"

scope :: Monad m => EnvT m a -> EnvT m a
scope c = do
  s <- get
  incCurrentScopeId
  nid <- getCurrentScopeId
  let e = Env nid Map.empty (Just (env s))
  (x, s') <- lift (runStateT c (s {env = e}))
  put s'
  case env s' of
    Env _ _ (Just s'') -> putEnv s''
    _ -> error "missing scope in nested environment?"
  return x

freshVarId :: Monad m => EnvT m VarId
freshVarId = updateNextVarId (+1)

freshRefId :: Monad m => EnvT m VarId
freshRefId = updateNextRefId (+1)

forceInsertRefMeta ::
  Monad m => VarId -> RefMeta -> EnvT m ()
forceInsertRefMeta i meta = do
  r <- forceLookupRef i
  modifyRefMap (IntMap.insert i (r, meta))

forceInsertExtern ::
  Monad m => VarId -> EnvT m ()
forceInsertExtern = modifyExternSet . IntSet.insert

forceInsertRef ::
  Monad m => Loc -> VarName -> Bool -> VarId -> Term -> EnvT m ()
forceInsertRef lo na isPure i t = do
  let meta = RefMeta
              { refMetaIsTerminationChecked = False
              , refMetaIsDeclaredPure = isPure
              , refMetaLoc = lo
              , refMetaName = na
              }
  modifyRefMap (IntMap.insert i (t, meta))

insertImplicitVarCurrentScope :: Monad m => VarId -> PreTerm -> EnvT m ()
insertImplicitVarCurrentScope i t = do
  x <- fmap envTrace getEnv
  modifyImplicitVarMap (IntMap.insert i (t, x))

clearImplicitVarMap :: Monad m => EnvT m ()
clearImplicitVarMap = modifyImplicitVarMap (\_ -> IntMap.empty)

forceInsertImplicit ::
  Monad m => VarId -> Implicits -> EnvT m ()
forceInsertImplicit i xs = modifyImplicitMap (IntMap.insert i xs)

forceInsertDataCtor ::
  Monad m => VarId -> [Var] -> EnvT m ()
forceInsertDataCtor i xs = modifyDataCtorMap (IntMap.insert i xs)

lookupRef :: Monad m => VarId -> EnvT m (Maybe Term)
lookupRef i = fmap (fmap fst . IntMap.lookup i) getRefMap

isExtern :: Monad m => VarId -> EnvT m Bool
isExtern i = fmap (IntSet.member i) getExternSet

forceLookupRef :: Monad m => VarId -> EnvT m Term
forceLookupRef i = do
  r <- getRefMap
  return (forceLookupRefMap i r)

forceLookupRefMeta :: Monad m => VarId -> EnvT m RefMeta
forceLookupRefMeta i = do
  r <- getRefMap
  return (forceLookupRefMetaMap i r)

forceLookupImplicit :: Monad m => VarId -> EnvT m Implicits
forceLookupImplicit i = do
  r <- getImplicitMap
  return (forceLookupImplicitMap i r)

isDataType :: Monad m => VarId -> EnvT m Bool
isDataType i = do
  r <- getDataCtorMap
  return (IntMap.member i r)

forceLookupDataCtor :: Monad m => VarId -> EnvT m [Var]
forceLookupDataCtor i = do
  r <- getDataCtorMap
  return (forceLookupDataCtorMap i r)

memberRef :: Monad m => VarId -> EnvT m Bool
memberRef = fmap isJust . lookupRef

forceLookupRefMapPair :: VarId -> RefMap -> (Term, RefMeta)
forceLookupRefMapPair i r =
  case IntMap.lookup i r of
    Nothing ->
      error $ "unable to find var id " ++ quote (show i) ++ " in ref map"
    Just x -> x

forceLookupRefMap :: VarId -> RefMap -> Term
forceLookupRefMap i r = fst (forceLookupRefMapPair i r)

forceLookupRefMetaMap :: VarId -> RefMap -> RefMeta
forceLookupRefMetaMap i r = snd (forceLookupRefMapPair i r)

forceLookupImplicitMap :: VarId -> ImplicitMap -> Implicits
forceLookupImplicitMap i r =
  case IntMap.lookup i r of
    Nothing ->
      error $ "unable to find var id " ++ quote (show i) ++ " in implicit map"
    Just x -> x

forceLookupDataCtorMap :: VarId -> DataCtorMap -> [Var]
forceLookupDataCtorMap i r =
  case IntMap.lookup i r of
    Nothing ->
      error $ "unable to find var id " ++ quote (show i) ++ " in data ctor map"
    Just x -> x
