{-# LANGUAGE BangPatterns #-}

module TypeCheck.Env
  ( VarStatus (..)
  , isStatusUnknown
  , isStatusInProgress
  , isStatusTerm
  , Env
  , EnvT
  , getEnv
  , ScopeMap
  , GlobalMap
  , forceInsertGlobal
  , includeImportBasePath
  , getImportBasePaths
  , getRefMap
  , getImplicitMap
  , getDataCtorMap
  , runEnvT
  , addToGlobals
  , findTermVarType
  , TypeCheck.Env.lookup
  , getModuleExpandMap
  , tryInsertModuleExpand
  , expandVarName
  , localEnvNewModule
  , forceInsert
  , tryInsert
  , tryUpdateIf
  , tryUpdate
  , getScopeMap
  , isInThisScope
  , scope
  , freshVarId
  , freshRefId
  , forceInsertRef
  , forceInsertExtern
  , forceInsertDataCtor
  , forceInsertUnfinishedData
  , removeUnfinishedData
  , forceLookupRefMap
  , forceLookupImplicit
  , isDataType
  , forceLookupDataCtor
  , lookupUnfinishedData
  , forceInsertImplicit
  , forceLookupImplicitMap
  , forceLookupDataCtorMap
  , forceInsertRefMeta
  , forceLookupRefMeta
  , addRootRefVar
  , getRootRefVars
  , lookupRef
  , isExtern
  , forceLookupRef
  , memberRef
  , toString
  , withDepth
  , getDepth
  , getNextLocalVarIdStr
  , getNextVarId
  , ImplicitVarMap
  , getImplicitVarMap
  , insertImplicitVarCurrentScope
  , clearImplicitVarMap
  )
where

import TypeCheck.Term
import TypeCheck.HackGlobalVarId
import Data.Maybe (isNothing, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State
import ParseTree
import Loc (Loc)
import Str (quote, operandSplit, operandConcatMaybe, varNameModuleSplit)
import Control.Exception (assert)

type EnvDepth = Int

type ScopeId = Int

-- Note:
-- Currently ctors and data types are never unknown after
-- type checking a module, even lazily. The below ctors should
-- support if lazy checking of ctors and data is needed at some point.
data VarStatus = StatusTerm Term
                -- StatusUnknownCtor:
                --  - SubstMap = delayed subst map
                --  - EnvDepth = environment depth of ctor
                --  - FilePath = Path for file containing ctor
                --  - FilePath = Corresponding output file base name
                --  - VarName = Module containing ctor
                --  - (Loc, VarName) of data type definition
                --  - Decl is ctor decl
               | StatusUnknownCtor SubstMap EnvDepth FilePath VarName FilePath (Loc, VarName) Decl
                -- StatusUnknownRef:
                --  - SubstMap = delayed subst map
                --  - EnvDepth = environment depth of ref
                --  - Bool = is data type?
                --  - FilePath = Path for file containing ref
                --  - FilePath = Corresponding output file base name
                --  - VarName = Module containing ref
                --  - Def = Definition to type check
               | StatusUnknownRef SubstMap EnvDepth Bool FilePath VarName FilePath Def
               | StatusUnknownVar SubstMap EnvDepth (Loc, VarName) VarId (Maybe Expr)
               -- StatusInProgress:
               --   - Bool = true if ctor
               --   - VarName = Module containing in progress definition
               --   - (Loc, VarName) of definition
               | StatusInProgress Bool VarName (Loc, VarName) 
               deriving Show

type UnfinishedDataMap = IntMap (EnvDepth, SubstMap, Def)

type GlobalMap = Map.Map VarName VarStatus

type ModuleExpandMap = Map.Map VarName VarName

-- Bool = true if name is to be exported.
type ScopeMap = Map.Map VarName (VarStatus, Bool)

data Env = Env ScopeId ScopeMap (Maybe Env) deriving Show

-- Module name -> (module's expand map, root environment of module)
type ModuleStateMap =
  Map.Map VarName (Set FilePath, ModuleExpandMap, Env)

type ImplicitVarMap = IntMap.IntMap (PreTerm, [ScopeId])

data EnvSt = EnvSt
  { env :: Env
  , globalEnv :: GlobalMap
  , importBasePaths :: Set FilePath
  , moduleExpandMap :: ModuleExpandMap
  , moduleStateMap :: ModuleStateMap
  , nextLocalVarName :: VarId
  , hackVarId :: VarId
  , nextRefId :: VarId
  , currentScopeId :: ScopeId
  , refMap :: RefMap
  , implicitVarMap :: ImplicitVarMap
  , rootRefVars :: [RefVar]
  , externSet :: ExternSet
  , implicitMap :: ImplicitMap
  , dataCtorMap :: DataCtorMap
  , unfinishedDataMap :: UnfinishedDataMap }
  deriving Show

type EnvT = StateT EnvSt

isStatusUnknown :: VarStatus -> Bool
isStatusUnknown (StatusUnknownCtor _ _ _ _ _ _ _) = True
isStatusUnknown (StatusUnknownRef _ _ _ _ _ _ _) = True
isStatusUnknown (StatusUnknownVar _ _ _ _ _) = True
isStatusUnknown _ = False

isStatusInProgress :: VarStatus -> Bool
isStatusInProgress (StatusInProgress _ _ _) = True
isStatusInProgress _ = False

isStatusTerm :: VarStatus -> Bool
isStatusTerm (StatusTerm _) = True
isStatusTerm _ = False

emptyCtxSt :: EnvSt
emptyCtxSt =
  EnvSt
    { env = Env 0 Map.empty Nothing
    , globalEnv = Map.empty
    , importBasePaths = Set.empty
    , moduleExpandMap = Map.empty
    , moduleStateMap = Map.empty
    , nextLocalVarName = 0
    , hackVarId = 0
    , nextRefId = 0
    , currentScopeId = 0
    , refMap = IntMap.empty
    , implicitVarMap = IntMap.empty
    , rootRefVars = []
    , externSet = IntSet.empty
    , implicitMap = IntMap.empty
    , dataCtorMap = IntMap.empty
    , unfinishedDataMap = IntMap.empty }

toString :: Monad m => EnvT m String
toString = fmap show getEnv

runEnvT :: Monad m => EnvT m () -> m ()
runEnvT s = evalStateT s emptyCtxSt

getCurrentScopeId :: Monad m => EnvT m ScopeId
getCurrentScopeId = fmap currentScopeId get

incCurrentScopeId :: Monad m => EnvT m ()
incCurrentScopeId = modify (\s -> s{currentScopeId = currentScopeId s + 1})

includeImportBasePath :: Monad m => FilePath -> EnvT m ()
includeImportBasePath p =
  modify (\s -> s{importBasePaths = Set.insert p (importBasePaths s)})

{-# INLINE getImportBasePaths #-}
getImportBasePaths :: Monad m => EnvT m (Set FilePath)
getImportBasePaths = fmap importBasePaths get

{-# INLINE getRefMap #-}
getRefMap :: Monad m => EnvT m RefMap
getRefMap = fmap refMap get

{-# INLINE getImplicitVarMap #-}
getImplicitVarMap :: Monad m => EnvT m ImplicitVarMap
getImplicitVarMap = fmap implicitVarMap get

getExternSet :: Monad m => EnvT m ExternSet
getExternSet = fmap externSet get

{-# INLINE getImplicitMap #-}
getImplicitMap :: Monad m => EnvT m ImplicitMap
getImplicitMap = fmap implicitMap get

getDataCtorMap :: Monad m => EnvT m DataCtorMap
getDataCtorMap = fmap dataCtorMap get

getUnfinishedDataMap :: Monad m => EnvT m UnfinishedDataMap
getUnfinishedDataMap = fmap unfinishedDataMap get

modifyRefMap :: Monad m => (RefMap -> RefMap) -> EnvT m ()
modifyRefMap f = modify (\s -> s{refMap = f (refMap s)})

modifyImplicitVarMap :: Monad m => (ImplicitVarMap -> ImplicitVarMap) -> EnvT m ()
modifyImplicitVarMap f = modify (\s -> s{implicitVarMap = f (implicitVarMap s)})

addRootRefVar :: Monad m => RefVar -> EnvT m ()
addRootRefVar v =
  modify (\s -> s{rootRefVars = v : rootRefVars s})

getRootRefVars :: Monad m => EnvT m [RefVar]
getRootRefVars = fmap rootRefVars get

modifyExternSet :: Monad m => (ExternSet -> ExternSet) -> EnvT m ()
modifyExternSet f = modify (\s -> s{externSet = f (externSet s)})

modifyImplicitMap :: Monad m => (ImplicitMap -> ImplicitMap) -> EnvT m ()
modifyImplicitMap f = modify (\s -> s{implicitMap = f (implicitMap s)})

modifyDataCtorMap :: Monad m => (DataCtorMap -> DataCtorMap) -> EnvT m ()
modifyDataCtorMap f = modify (\s -> s{dataCtorMap = f (dataCtorMap s)})

modifyUnfinishedDataMap :: Monad m =>
  (UnfinishedDataMap -> UnfinishedDataMap) -> EnvT m ()
modifyUnfinishedDataMap f =
  modify (\s -> s{unfinishedDataMap = f (unfinishedDataMap s)})

getNextLocalVarIdStr :: Monad m => EnvT m VarName
getNextLocalVarIdStr = do
  st <- get
  let n = nextLocalVarName st
  put (st {nextLocalVarName = n + 1})
  return (show n)

{-# NOINLINE getNextVarId #-}
getNextVarId :: Monad m => EnvT m VarId
getNextVarId = do
  st <- get
  let !(n, i') = hackReadGlobalVarId (hackVarId st)
  put (st {hackVarId = i' + 1})
  return n

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

getGlobalEnv :: Monad m => EnvT m GlobalMap
getGlobalEnv = fmap globalEnv get

modifyGlobalEnv :: Monad m => (GlobalMap -> GlobalMap) -> EnvT m ()
modifyGlobalEnv f = modify (\s -> s{globalEnv = f (globalEnv s)})

putGlobalEnv :: Monad m => GlobalMap -> EnvT m ()
putGlobalEnv g = modifyGlobalEnv (const g)

forceInsertGlobal :: Monad m => VarName -> VarStatus -> EnvT m ()
forceInsertGlobal na t = modifyGlobalEnv (Map.insert na t)

getEnv :: Monad m => EnvT m Env
getEnv = fmap env get

modifyEnv :: Monad m => (Env -> Env) -> EnvT m ()
modifyEnv f = modify (\s -> s{env = f (env s)})

putEnv :: Monad m => Env -> EnvT m ()
putEnv s = modifyEnv (const s)

getModuleStateMap :: Monad m => EnvT m ModuleStateMap
getModuleStateMap = fmap moduleStateMap get

modifyModuleStateMap :: Monad m =>
  (ModuleStateMap -> ModuleStateMap) -> EnvT m ()
modifyModuleStateMap f =
  modify (\s -> s{moduleStateMap = f (moduleStateMap s)})

lookupOrCreateModuleStateMap :: Monad m =>
  VarName -> EnvT m (Set FilePath, ModuleExpandMap, Env)
lookupOrCreateModuleStateMap modName = do
  m <- getModuleStateMap
  case Map.lookup modName m of
    Nothing -> do
      incCurrentScopeId
      nid <- getCurrentScopeId
      let p = (Set.empty, Map.empty, Env nid Map.empty Nothing)
      modifyModuleStateMap (Map.insert modName p)
      return p
    Just p -> return p

getModuleExpandMap :: Monad m => EnvT m ModuleExpandMap
getModuleExpandMap = fmap moduleExpandMap get

tryInsertModuleExpand :: Monad m => VarName -> VarName -> EnvT m Bool
tryInsertModuleExpand alias mo = do
  s <- get
  let m = moduleExpandMap s
  if Map.member alias m
  then return False
  else do
    put (s {moduleExpandMap = Map.insert alias mo m})
    return True

-- Assumes that newModName is different from the current module!
localEnvNewModule :: Monad m => VarName -> EnvT m a -> EnvT m a
localEnvNewModule newModName c = do
  s <- get
  (fs, ex, en) <- lookupOrCreateModuleStateMap newModName
  msm <- getModuleStateMap
  (x, s') <- lift (runStateT c
                    (s { moduleStateMap = msm
                       , importBasePaths = fs
                       , moduleExpandMap = ex
                       , env = en }))
  let msm' = moduleStateMap s'
  put (s' { moduleStateMap =
              Map.insert newModName
                (importBasePaths s', moduleExpandMap s', env s') msm'
          , importBasePaths = importBasePaths s
          , moduleExpandMap = moduleExpandMap s
          , env = env s })
  return x

expandModuleName :: Monad m => VarName -> EnvT m VarName
expandModuleName mn = do
  ma <- fmap moduleExpandMap get
  case Map.lookup mn ma of
    Nothing -> return mn
    Just ex -> return ex

expandVarName :: Monad m => VarName -> VarName -> EnvT m VarName
expandVarName modName vn = do
  let (vn', vm) = operandSplit vn
  let (v1, w2) = varNameModuleSplit vn'
  w <- case w2 of
        Nothing -> return v1
        Just v2 -> do
          v2' <- expandModuleName v2
          return (v1 ++ "." ++ v2')
  case vm of
    Nothing -> return w
    Just vm' -> do
      wm <- expandVarName modName vm'
      s <- TypeCheck.Env.lookup modName wm
      case s of
        Just (StatusTerm t) -> do
          case termPre t of
            TermData d -> return (w ++ "#" ++ varName d)
            _ -> return (w ++ "#" ++ wm)
        Just (StatusUnknownRef _ _ _ _ refModule _ _) ->
          return (w ++ "#" ++ addModule refModule wm)
        Just (StatusInProgress _ refModule _) ->
          return (w ++ "#" ++ addModule refModule wm)
        _ -> return (w ++ "#" ++ wm)
  where
    addModule :: VarName -> VarName -> VarName
    addModule moduleToAdd ss =
      let (s0, s1) = operandSplit ss
          s0' = case moduleToAdd of
                  "" -> s0
                  _ -> s0 ++ "." ++ moduleToAdd
      in operandConcatMaybe s0' s1

findTermVarType :: Var -> Env -> Maybe PreTerm
findTermVarType v (Env _ m next) =
  case Map.lookup (varName v) m of
    Just (StatusTerm t@(Term {termPre = TermVar _ w}), _) ->
        if varId v == varId w
        then Just (termTy t)
        else next >>= findTermVarType v
    _ -> next >>= findTermVarType v

doLookup :: GlobalMap -> VarName -> Env -> Maybe VarStatus
doLookup g v (Env _ m Nothing) =
  case Map.lookup v m of
    Nothing -> Map.lookup v g
    Just (x, _) -> Just x
doLookup g v (Env _ m (Just p)) =
  case Map.lookup v m of
    Nothing -> doLookup g v p
    Just (e, _) -> Just e

lookup :: Monad m => VarName -> VarName -> EnvT m (Maybe VarStatus)
lookup modName n = do
  n' <- expandVarName modName n
  g <- getGlobalEnv
  fmap (doLookup g n') getEnv

doForceInsert :: VarName -> VarStatus -> Bool -> Env -> Env
doForceInsert n x b (Env sid m p) = Env sid (Map.insert n (x, b) m) p

forceInsert :: Monad m => VarName -> VarStatus -> Bool -> EnvT m ()
forceInsert n p b = modifyEnv (doForceInsert n p b)

doTryInsert :: VarName -> VarStatus -> Bool -> Env -> Either VarStatus Env
doTryInsert n x b (Env sid m p) =
  case Map.lookup n m of
    Nothing -> Right (Env sid (Map.insert n (x, b) m) p)
    Just (s, _) -> Left s

tryInsert ::
  Monad m => VarName -> VarStatus -> Bool -> EnvT m (Maybe VarStatus)
tryInsert n x b = do
  a <- fmap (doTryInsert n x b) getEnv
  case a of
    Right c -> putEnv c >> return Nothing
    Left s -> return (Just s)

getScopeMap :: Monad m => EnvT m ScopeMap
getScopeMap = do
  Env _ em _ <- getEnv
  return em

addToGlobals :: Monad m => VarName -> EnvT m ()
addToGlobals moduleName = do
  Env _ em p <- getEnv
  let !() = assert (isNothing p) ()
  g <- getGlobalEnv
  putGlobalEnv $! (Map.foldrWithKey insertGlob g em)
  where
    insertGlob :: VarName -> (VarStatus, Bool) -> GlobalMap -> GlobalMap
    insertGlob na (s, True) g =
      Map.insert (insertModuleName na) s g
    insertGlob _ _ g = g

    insertModuleName :: VarName -> VarName
    insertModuleName na =
      let (start, end) = span (/= '#') na
      in start ++ "." ++ moduleName ++ end

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
    Just (s, b) -> if p s
                   then Just (Env sid (Map.insert n (x, b) m) c, s)
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
  origNextVar <- fmap nextLocalVarName get
  depth <- getDepth
  s <- if depth == 0
        then do
          s0 <- get
          return (s0 {nextLocalVarName = 0})
        else
          get
  incCurrentScopeId
  nid <- getCurrentScopeId
  let e = Env nid Map.empty (Just (env s))
  (x, s') <- lift (runStateT c (s {env = e}))
  if depth == 0
  then put (s' {nextLocalVarName = origNextVar})
  else put s'
  case env s' of
    Env _ _ (Just s'') -> putEnv s''
    _ -> error "missing scope in nested environment?"
  return x

freshVarId :: Monad m => EnvT m VarId
freshVarId = do
  st <- get
  let !(n, i') = hackFreshGlobalVarId (hackVarId st)
  put (st {hackVarId = i' + 1})
  return n
{-# NOINLINE freshVarId #-}

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
  depth <- getDepth
  let meta = RefMeta
              { refMetaIsTerminationChecked = False
              , refMetaIsDeclaredPure = isPure
              , refMetaLoc = lo
              , refMetaName = na
              , refMetaIsGlobal = depth == 0
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

forceInsertUnfinishedData ::
  Monad m => VarId -> EnvDepth -> SubstMap -> Def -> EnvT m ()
forceInsertUnfinishedData i depth su def =
  modifyUnfinishedDataMap (IntMap.insert i (depth, su, def))

removeUnfinishedData :: Monad m => VarId -> EnvT m ()
removeUnfinishedData = modifyUnfinishedDataMap . IntMap.delete

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

lookupUnfinishedData :: Monad m =>
  VarId -> EnvT m (Maybe (EnvDepth, SubstMap, Def))
lookupUnfinishedData i = fmap (IntMap.lookup i) getUnfinishedDataMap

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
