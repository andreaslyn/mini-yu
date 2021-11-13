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
  | Case Var [(CtorId, FieldCnt, Expr)]
  | ConstRef Const
  | CtorRef Ctor [Var]
  | Proj Int Var
  | Let Var Expr Expr
  | Ret Var
  | Where Expr ProgramRoots
  | Hidden Bool -- Bool = False if function/lazy, and True otherwise.

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
    freev (Hidden _) = return IntSet.empty
    freev (Extern _) = return IntSet.empty
    freev (Lazy _ e) = freev e
    freev (Case v cs) = do
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
  , teModuleName :: Te.VarName
  , teDataCtorMap :: Te.DataCtorMap
  , program :: Program
  , constMap :: IntMap Const -- Te ref id -> Const
  , varMap :: IntMap Var     -- Te var id -> Var
  , ctorMap :: IntMap Ctor   -- Te ctor id -> Ctor
  , identityDataTypeSet :: IntSet
  , nextConst :: Int
  , nextVar :: Int
  , unitConst :: Const
  , optimize :: Bool
  }

type StM = State St

highLevelIr ::
  Bool -> Te.VarName ->
  [Te.RefVar] -> Te.DataCtorMap -> Te.ImplicitMap -> Te.RefMap ->
  (Program, ProgramRoots)
highLevelIr = runStM

convertName :: String -> String
convertName n =
  let (n0, n1) = Str.operandSplit n
  in case n1 of
      Nothing -> n0
      Just n1' ->
        if null n1'
        then n0
        else n0 ++ Str.operandDelim ++ convertName n1'

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

getModuleName :: StM Te.VarName
getModuleName = fmap teModuleName get

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

concatRelevantDom :: 
  Te.RefMap -> Te.Implicits -> Te.PreTerm ->
  Maybe [(Maybe Te.Var, Te.PreTerm)]
concatRelevantDom rm is ty = do
  dom <- concatDom rm is ty
  return (filter (\(_, dty) -> isRelevantTy rm dty) dom)

concatDom ::
  Te.RefMap -> Te.Implicits -> Te.PreTerm ->
  Maybe [(Maybe Te.Var, Te.PreTerm)]
concatDom rm [] ty = concatDomAux rm ty
concatDom rm is ty =
  let is' = map (\(v,t) -> (Just v, t)) is
  in case concatDomAux rm ty of
      Nothing -> return is'
      Just d -> return (is' ++ d)

concatDomAux ::
  Te.RefMap -> Te.PreTerm -> Maybe [(Maybe Te.Var, Te.PreTerm)]
concatDomAux rm t =
  case TE.preTermDomCod rm t of
    Nothing ->
      case TE.preTermLazyCod rm t of
        Nothing -> Nothing
        Just (t', _) -> concatDomAux rm t'
    Just (d, c, _) ->
      case concatDomAux rm c of
        Nothing -> Just d
        Just d' -> Just (d ++ d')

insertIdentityDataTypeSet :: Te.Var -> StM ()
insertIdentityDataTypeSet d = do
  modify $ \st ->
    st {identityDataTypeSet =
          IntSet.insert (Te.varId d) (identityDataTypeSet st)}

memberIdentityDataTypeSet :: Te.Var -> StM Bool
memberIdentityDataTypeSet d = do
  s <- fmap identityDataTypeSet get
  return (IntSet.member (Te.varId d) s)

updateIdentityDataTypeFromCtors :: Te.Var -> [Te.Var] -> StM ()
updateIdentityDataTypeFromCtors dat [ctor] = do
  rm <- getRefMap
  is <- lookupImplicit (Te.varId ctor)
  tm <- lookupRef (Te.varId ctor)
  case concatRelevantDom rm is (Te.termTy tm) of
    Just [_] -> insertIdentityDataTypeSet dat
    _ -> return ()
updateIdentityDataTypeFromCtors _ _ = return ()

runStM ::
  Bool -> Te.VarName ->
  [Te.RefVar] -> Te.DataCtorMap -> Te.ImplicitMap -> Te.RefMap ->
  (Program, ProgramRoots)
runStM enableOptimization moduleName vs dm im rm =
  let st = St { teImplicitMap = im
              , teRefMap = rm
              , teModuleName = moduleName
              , teDataCtorMap = dm
              , program = IntMap.empty
              , constMap = IntMap.empty
              , varMap = IntMap.empty
              , ctorMap = IntMap.empty
              , identityDataTypeSet = IntSet.empty
              , nextConst = 0
              , nextVar = 0
              , unitConst = Const "" 0 -- Dummy value in the beginning.
              , optimize = enableOptimization
              }
      (x, st') = runState (irInitProgram vs >>= optimizeProgram) st
  in (program st', x)

irInitProgram :: [Te.RefVar] -> StM ProgramRoots
irInitProgram vs = do
  unitC <- getNextConst False Str.unitName
  modify (\st -> st {unitConst = unitC})
  rs <- irProgram True vs
  p1 <- fmap program get
  let unitR = CtorRef unitCtor []
  modify (\st -> st {program = IntMap.insert (prConst unitC) (unitC, unitR, True) p1})
  return (unitC : rs)

irProgram :: Bool -> [Te.RefVar] -> StM ProgramRoots
irProgram isStatic vs = do
  mapM_ updateRefMap vs
  ps <- doIrProgram isStatic vs
  st <- get
  p <- fmap program get
  let p' = foldl (\m (c,x) -> IntMap.insert (prConst c) (c, x, isStatic) m) p ps
  put (st {program = p'})
  return (map fst ps)

isInThisModule :: Te.Var -> StM Bool
isInThisModule v =
  case Str.moduleSplit (Te.varName v) of
    (_, Nothing) -> return True
    (_, Just m) -> fmap (== m) getModuleName

hiddenFromRef :: Te.Var -> StM Expr
hiddenFromRef v = do
  r <- lookupRef (Te.varId v)
  case Te.termPre r of
    Te.TermFun _ _ _ _ -> return (Hidden False)
    Te.TermLazyFun _ _ -> return (Hidden False)
    _ -> return (Hidden True)

hiddenFromRefTy :: Te.Var -> StM Expr
hiddenFromRefTy v = do
  t <- lookupRef (Te.varId v)
  is <- lookupImplicit (Te.varId v)
  if not (null is)
  then return (Hidden False)
  else do
    rm <- getRefMap
    case TE.preTermDomCod rm (Te.termTy t) of
      Nothing ->
        case TE.preTermLazyCod rm (Te.termTy t) of
          Nothing -> return (Hidden True)
          Just _ -> return (Hidden False)
      Just _ -> return (Hidden False)

doIrProgram :: Bool -> [Te.RefVar] -> StM [(Const, Expr)]
doIrProgram _ [] = return []
doIrProgram isStatic (Te.RefExtern v a : vs) = do
  m <- doIrProgram isStatic vs
  c <- lookupTermRef v
  inm <- isInThisModule v
  if isStatic && not inm
  then return ((c, Hidden False) : m)
  else return ((c, Extern a) : m)
doIrProgram isStatic (Te.RefVal v : vs) = do
  inm <- isInThisModule v
  e <- if isStatic && not inm
       then hiddenFromRef v
       else irRef v
  m <- doIrProgram isStatic vs
  c <- lookupTermRef v
  return ((c, e) : m)
doIrProgram isStatic (Te.RefData dv : vs) = do
  inm <- isInThisModule dv
  e <- if isStatic && not inm
       then hiddenFromRefTy dv
       else makeCtorConstFromVar Nothing dv dataCtor
  ctors <- lookupDataCtor (Te.varId dv)
  ctors' <- mapM (irCtor inm) ctors
  m <- doIrProgram isStatic vs
  c <- lookupTermRef dv
  return ((c, e) : ctors' ++ m)
  where
    irCtor :: Bool -> Te.Var -> StM (Const, Expr)
    irCtor inm v = do
      e <- if isStatic && not inm
           then hiddenFromRefTy v
           else do
            ctor <- lookupTermCtor (Te.varId v)
            makeCtorConstFromVar (Just dv) v ctor
      c <- lookupTermRef v
      return (c, e)

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
      updateIdentityDataTypeFromCtors dv cs
      getNextConst True (Te.varName dv) >>= updateTermRef dv
      mapM_ updateCtor (zip cs [0..])
    _ -> error "expected data from RefData"
  where
    updateCtor :: (Te.Var, Int) -> StM ()
    updateCtor (c, i) = do
      getNextConst True (Te.varName c) >>= updateTermRef c
      updateTermCtor c (Ctor i)

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

makeCtorConstFromVar :: Maybe Te.Var -> Te.Var -> Ctor -> StM Expr
makeCtorConstFromVar dat v ctor = do
  t <- lookupRef (Te.varId v)
  is <- lookupImplicit (Te.varId v)
  if null is
    then makeCtorConst dat ctor (Te.termTy t)
    else
      let as = map (\(i,x) -> (Just i, x)) is
      in makeCtorConst dat ctor (Te.TermArrow False as (Te.termTy t))

makeCtorConst :: Maybe Te.Var -> Ctor -> Te.PreTerm -> StM Expr
makeCtorConst dat ctor preTerm = do
  b <- case dat of
        Nothing -> return False
        Just dat' -> memberIdentityDataTypeSet dat'
  make b [] preTerm
  where
    make :: Bool -> [[Var]] -> Te.PreTerm -> StM Expr
    make isIden as t = do
      rm <- getRefMap
      case TE.preTermDomCod rm t of
        Nothing ->
          case TE.preTermLazyCod rm t of
            Nothing ->
              let ras = concat (reverse as) in
              if isIden
              then
                case ras of
                  [a] -> return (Ret a)
                  _ -> error "multiple relevant arguments on identity data ctor"
              else
                return (CtorRef ctor ras)
            Just (cod, _) -> fmap (Lazy False) (make isIden as cod)
        Just (dom, cod, _) -> do
          a <- makeFunRVars snd dom
          c <- make isIden (filterRelevantRVars a : as) cod
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
irExpr (Te.TermCase e ct) = do
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
irCaseTree xs (Te.CaseLeaf vs te ws) = do
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
  case allCases of
    [(_, Nothing, singleCase)] ->
      return singleCase
    _ ->
      let allCases' = map (\ (i, n, e) -> (i, fromJust n, e)) allCases
      in return (Case x allCases')
irCaseTree xs (Te.CaseUnit idx (vs, ct)) = do
  let (x, xs') = removeIdx idx xs
  localUpdateTermVars vs (repeat x) $ do
    ct' <- irCaseTree xs' ct
    return (Case x [(0, 0, ct')])
irCaseTree xs (Te.CaseEmpty idx) = do
  let (x, _) = removeIdx idx xs
  return (Case x [])

makeCtorCase ::
  Bool -> Var -> [Var] -> (Te.VarId, ([Te.Var], Te.CaseTree)) ->
  StM (Te.VarId, (CtorId, Maybe FieldCnt, Expr))
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
        return (Te.varId dataId, (prCtor i', Just 0, c))
      Just dom -> do
        if makeProjects
        then do
          ys0 <- makeFunRVars snd dom
          isIden <- memberIdentityDataTypeSet dataId
          (ys, p) <- makeProjs isIden 0 dataId (zip ys0 dom)
          c <- irCaseTree (ys ++ xs') ct
          let dom' = filter (\(_, dty) -> isRelevantTy rm dty) dom
          let !() = assert (not isIden || length dom' == 1) ()
          let nfields = if isIden then Nothing else Just (length dom')
          return (Te.varId dataId, (prCtor i', nfields, p c))
        else do
          c <- irCaseTree xs' ct
          let dom' = filter (\(_, dty) -> isRelevantTy rm dty) dom
          return (Te.varId dataId, (prCtor i', Just (length dom'), c))
  where
    makeProjs ::
      Bool -> Int -> Te.Var -> [(RVar, (a, Te.PreTerm))] ->
      StM ([Var], Expr -> Expr)
    makeProjs _ _ _ [] = return ([], id)
    makeProjs isIden n dataId ((Irrelevant y', (_, t)) : ys) = do
      (ys', p) <- makeProjs isIden n dataId ys
      dataVal <- makeIrrelevantFunNormalized t
      return (y' : ys', Let y' dataVal . p)
    makeProjs isIden n dataId ((Relevant y', _) : ys) = do
      (ys', p) <- makeProjs isIden (n + 1) dataId ys
      if isIden
      then return (x : ys', p)
      else return (y' : ys', Let y' (Proj n x) . p)

removeIdx :: Int -> [a] -> (a, [a])
removeIdx 0 (x : xs) = (x, xs)
removeIdx i (x : xs) =
  let (x', xs') = removeIdx (i-1) xs in (x', x : xs')
removeIdx _ _ = error "invalid index to removeIdx IR"


--------------------- Optimization -------------------------


optimizeProgram :: ProgramRoots -> StM ProgramRoots
optimizeProgram roots = do
  b <- fmap optimize get
  if b
  then doOptimizeProgram roots
  else return roots

doOptimizeProgram :: ProgramRoots -> StM ProgramRoots
doOptimizeProgram roots = do
  simplLazyAndDelayForce roots
  return roots

simplLazyAndDelayForce :: ProgramRoots -> StM ()
simplLazyAndDelayForce [] = return ()
simplLazyAndDelayForce (c : rs) = do
  p0 <- fmap program get
  let (e, isStatic) = lookupConst c p0
  e' <- simplLazyAndDelayForceExpr e
  p1 <- fmap program get
  let p2 = IntMap.insert (prConst c) (c, e', isStatic) p1
  modify (\st -> st {program = p2})
  simplLazyAndDelayForce rs

simplLazyAndDelayForceExpr :: Expr -> StM Expr
simplLazyAndDelayForceExpr (Lazy io e) = do
  e' <- simplLazyAndDelayForceExpr e
  p <- fmap program get
  case removeLazyForce p e of
    Nothing -> return (Lazy io e')
    Just d -> return d
simplLazyAndDelayForceExpr (Fun [] e) = do
  e' <- simplLazyAndDelayForceExpr e
  p <- fmap program get
  case removeDelayForce p e of
    Nothing -> return (Fun [] e')
    Just d -> return d
simplLazyAndDelayForceExpr (Fun vs e) = do
  e' <- simplLazyAndDelayForceExpr e
  return (Fun vs e')
simplLazyAndDelayForceExpr e@(Ap _ _ _) = return e
simplLazyAndDelayForceExpr e@(Force _ _) = return e
simplLazyAndDelayForceExpr e@(Hidden _) = return e
simplLazyAndDelayForceExpr e@(Extern _) = return e
simplLazyAndDelayForceExpr (Case v cs) = do
  cs' <-  mapM (\(i, n, e) -> fmap ((,,) i n) (simplLazyAndDelayForceExpr e)) cs
  return (Case v cs')
simplLazyAndDelayForceExpr e@(ConstRef _) = return e
simplLazyAndDelayForceExpr e@(CtorRef _ _) = return e
simplLazyAndDelayForceExpr e@(Proj _ _) = return e
simplLazyAndDelayForceExpr (Let v e1 e2) = do
  e1' <- simplLazyAndDelayForceExpr e1
  e2' <- simplLazyAndDelayForceExpr e2
  return (Let v e1' e2')
simplLazyAndDelayForceExpr e@(Ret _) = return e
simplLazyAndDelayForceExpr (Where e rs) = do
  e' <- simplLazyAndDelayForceExpr e
  simplLazyAndDelayForce rs
  return (Where e' rs)

removeLazyForce :: Program -> Expr -> Maybe Expr
removeLazyForce prog (Let v1 (Ret v2) e) = do
  e' <- removeLazyForce prog e
  Just (Let v1 (Ret v2) e')
removeLazyForce prog (Let v (ConstRef c) e) = do
  let (t, _) = lookupConst c prog
  case t of
    Lazy _ _ -> do
      e' <- removeLazyForce prog e
      Just (Let v (ConstRef c) e')
    _ -> Nothing
removeLazyForce _ (Force _ v) = Just (Ret v)
removeLazyForce _ _ = Nothing

removeDelayForce :: Program -> Expr -> Maybe Expr
removeDelayForce prog (Let v1 (Ret v2) e) = do
  e' <- removeDelayForce prog e
  Just (Let v1 (Ret v2) e')
removeDelayForce prog (Let v (ConstRef c) e) = do
  let (t, _) = lookupConst c prog
  case t of
    Fun _ _ -> do
      e' <- removeDelayForce prog e
      Just (Let v (ConstRef c) e')
    _ -> Nothing
removeDelayForce _ (Ap _ v []) = Just (Ret v)
removeDelayForce _ _ = Nothing

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
isLetOrCaseExpr (Case _ _) = True
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
writeExpr _ _ (Hidden b) = writeStr ("hidden-" ++ show b)
writeExpr _ _ (Extern a) = writeStr ("extern " ++ show a)
writeExpr _ p (Lazy _ e) = writeStr "[]. " >> writeExpr True p e
writeExpr inci p (Case v cs) = do
  when inci incIndent
  newLine
  writeStr "case "
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
      writeStr "of "
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
