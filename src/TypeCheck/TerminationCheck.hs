{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}

module TypeCheck.TerminationCheck
  ( terminationCheck
  , strictPositivityCheck
  , verifyRecursiveImpureChains
  , verifyDataImpureChains
  , verifyImpureIsNotTerminationChecked
  )
where

import Str (quote)
import Loc (Loc, loc)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Data.Set (Set)
import TypeCheck.TypeCheckT
import TypeCheck.Term
import TypeCheck.TermEnv
import Data.Foldable (foldlM, foldrM)
import Data.Maybe (isNothing, fromJust)
import qualified Data.List as List
import qualified TypeCheck.Env as Env
import qualified Data.Matrix as Matrix
import qualified Data.Vector as Vector
import Data.Matrix (Matrix)
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Except
import Control.Exception (assert)
import TypeCheck.PatternUnify
import TypeCheck.SubstMap
import Debug.Trace (trace)

_useTrace :: String -> a -> a
_useTrace = trace

data CallRel = CallSmaller | CallEqual | CallUnknown deriving Eq

instance Show CallRel where
  show CallSmaller = "(<)"
  show CallEqual = "(=)"
  show CallUnknown = "(?)"

callRelMult :: CallRel -> CallRel -> CallRel
callRelMult _ CallUnknown = CallUnknown
callRelMult CallUnknown _ = CallUnknown
callRelMult CallEqual CallEqual = CallEqual
callRelMult _ _ = CallSmaller

callRelPlus :: CallRel -> CallRel -> CallRel
callRelPlus _ CallSmaller = CallSmaller
callRelPlus CallSmaller _ = CallSmaller
callRelPlus CallUnknown CallUnknown = CallUnknown
callRelPlus _ _ = CallEqual

instance Num CallRel where
  (+) = callRelPlus
  (*) = callRelMult
  negate = id
  abs = id
  signum = id
  fromInteger 0 = CallUnknown
  fromInteger 1 = CallEqual
  fromInteger _ = CallSmaller

instance Ord CallRel where
  CallUnknown <= _ = True
  CallEqual <= CallUnknown = False
  CallEqual <= _ = True
  CallSmaller <= CallSmaller = True
  CallSmaller <= _ = False

multLessThanEq :: [CallRel] -> Bool
multLessThanEq [] = False
multLessThanEq (CallUnknown : _) = False
multLessThanEq (CallEqual : xs) = multLessThanEq xs
multLessThanEq (CallSmaller : _) = True

type CallMatrix = Matrix CallRel

instance Ord CallMatrix where
  compare m1 m2 = compare (Matrix.toList m1) (Matrix.toList m2)

type CallMap = IntMap (Set (VarId, CallMatrix))
        -- Caller -> Set (Callee, CallMatrix)

type CallMapT m = StateT ([Var], CallMap) (TypeCheckT m)

completeCallMap :: CallMap -> CallMap
completeCallMap cm =
  let cm' = stepCompleteCallMap cm (IntMap.toList cm)
  in if cm' == cm then cm else completeCallMap cm'

stepCompleteCallMap ::
  CallMap -> [(VarId, Set (VarId, CallMatrix))] -> CallMap
stepCompleteCallMap cm [] = cm
stepCompleteCallMap cm ((i, s) : is) =
  let cm' = foldl (\a (j, m) ->
              let s' = fromJust (IntMap.lookup j cm)
              in foldl (\b (k, n) ->
                         let s'' = fromJust (IntMap.lookup i b)
                             t = Set.insert (k, Matrix.multStd n m) s''
                         in IntMap.insert i t b
                       ) a s') cm s
  in stepCompleteCallMap cm' is

removeIrrelevant :: [[CallRel]] -> [[CallRel]]
removeIrrelevant rss =
  List.transpose $ filter hasSmaller (List.transpose rss)
  where
    hasSmaller :: [CallRel] -> Bool
    hasSmaller = List.any (== CallSmaller)

checkPermutations :: Int -> [[[CallRel]]] -> Bool
checkPermutations _ ([] : _) = False
checkPermutations n pss =
  let ps = map head pss
      pss' = map tail pss
      -- !() = trace ("check permutation: " ++ show ps) ()
  in checkPermutation 1 ps || checkPermutations n pss'
  where
    checkPermutation :: Int -> [[CallRel]] -> Bool
    checkPermutation i ps =
      if i > n
      then False
      else
        let ps' = map (take i) ps
        in and (map multLessThanEq ps') || checkPermutation (i + 1) ps

terminationCheck :: Monad m => Loc -> Var -> TypeCheckT m ()
terminationCheck lo v = do
  rm <- Env.getRefMap
  let t = Env.forceLookupRefMap (varId v) rm
  let rs = preTermRefs rm (termPre t)
  if not (IntSet.member (varId v) rs)
  then do
    istt <- allTypeChecked (IntSet.toList rs)
    if istt
    then do
      meta <- Env.forceLookupRefMeta (varId v)
      errWhenNotPure meta
      let meta' = meta {refMetaIsTerminationChecked = True}
      Env.forceInsertRefMeta (varId v) meta'
    else
      doTerminationCheck lo v
  else
    doTerminationCheck lo v
  where
    allTypeChecked :: Monad m => [VarId] -> TypeCheckT m Bool
    allTypeChecked rs =
      foldrM (\r a -> fmap (a &&) (isTypeChecked r)) True rs

    isTypeChecked :: Monad m => VarId -> TypeCheckT m Bool
    isTypeChecked i = do
      x <- Env.lookupRef i
      case x of
        Nothing -> Env.isExtern i
        Just _ -> return True

    errWhenNotPure :: Monad m => RefMeta -> TypeCheckT m ()
    errWhenNotPure meta =
      when (not (refMetaIsDeclaredPure meta)) $
        err lo
          (Fatal $
            quote (refMetaName meta) ++ " declared with with "
             ++ quote ("val.. " ++ refMetaName meta)
             ++ ", but can be declared with "
             ++ quote ("val " ++ refMetaName meta)
             ++ ", since it passed termination check!")

doTerminationCheck :: Monad m => Loc -> Var -> TypeCheckT m ()
doTerminationCheck lo v = do
  cm <- fmap completeCallMap (makeCallMap v)
  let s0 = Set.toList (fromJust (IntMap.lookup (varId v) cm))
  let s1 = filter (\x -> fst x == varId v) s0
  let s = map ((\x -> Vector.toList (Matrix.getDiag (snd x)))) s1
  --let !() = trace ("calls for " ++ varName v ++ ": " ++ show s) ()
  meta <- Env.forceLookupRefMeta (varId v)
  tt <- case s of
          [] -> return True
          (x:_xs) -> do
            let !() = assert (and (map (== length x) (map length _xs))) ()
            case removeIrrelevant s of
              [] -> errWhenPure meta >> return False
              r@(y:_) -> do
                let p = map List.permutations r
                let n = length y
                if not (checkPermutations n p)
                then errWhenPure meta >> return False
                else return True
  let meta' = meta {refMetaIsTerminationChecked = tt}
  Env.forceInsertRefMeta (varId v) meta'
  where
    errWhenPure :: Monad m => RefMeta -> TypeCheckT m ()
    errWhenPure meta =
      when (refMetaIsDeclaredPure meta)
        (err lo (Fatal $
          "termination check for " ++ quote (varName v) ++ " failed, \
          \declare with "
            ++ quote ("val.. " ++ varName v)
            ++ " to disable this check"))

makeCallMap :: Monad m => Var -> TypeCheckT m CallMap
makeCallMap v =
  fmap snd (execStateT makeCallMapLoop ([v], IntMap.empty))

getCallMap :: Monad m => CallMapT m CallMap
getCallMap = fmap snd get

putCallMap :: Monad m => CallMap -> CallMapT m ()
putCallMap m = modify (\(x, _) -> (x, m))

getCallStack :: Monad m => CallMapT m [Var]
getCallStack = fmap fst get

pushCallStack :: Monad m => Var -> CallMapT m ()
pushCallStack v = do
  vs <- getCallStack
  putCallStack (v : vs)

putCallStack :: Monad m => [Var] -> CallMapT m ()
putCallStack vs = modify (\(_, x) -> (vs, x))

makeCallMapLoop :: Monad m => CallMapT m ()
makeCallMapLoop = do
  s <- getCallStack
  case s of
    [] -> return ()
    v : vs -> putCallStack vs >> makeCallMapRef v >> makeCallMapLoop

makeCallMapRef :: Monad m => Var -> CallMapT m ()
makeCallMapRef v = do
  cm <- getCallMap
  if IntMap.member (varId v) cm
  then return ()
  else do
    r0 <- lift (Env.lookupRef (varId v))
    case r0 of
      Nothing -> do
        putCallMap (IntMap.insert (varId v) Set.empty cm)
      Just r -> do
        rm <- lift Env.getRefMap
        let fs0 = IntSet.elems (preTermVars rm (termPre r))
        fs <- lift (makeArgVars fs0)
        let rsu = IntMap.fromList (zip fs0 fs)
        case substPreTerm rsu (termPre r) of
          TermFun ias _ n ct -> do
            ias' <- lift (makeArgVars ias)
            as <- case n of
                    Nothing -> return []
                    Just n' -> lift (makeArgVars [1..n'])
            s <- runCaseTreeCallSet ct fs (ias' ++ as)
            putCallMap (IntMap.insert (varId v) s cm)
          r' -> do
            s <- runPreTermCallSet r' fs
            putCallMap (IntMap.insert (varId v) s cm)

makeArgVars :: Monad m => [a] -> TypeCheckT m [PreTerm]
makeArgVars [] = return []
makeArgVars (_ : vs) = do
  i <- Env.freshVarId
  let v' = mkVar i "_"
  vs' <- makeArgVars vs
  return (TermVar False v' : vs')

----------------------------------------------------------------------

type CallSetT m = ReaderT [PreTerm] (CallMapT m)

runPreTermCallSet :: Monad m =>
  PreTerm -> [PreTerm] -> CallMapT m (Set (VarId, CallMatrix))
runPreTermCallSet r vars =
  runReaderT (preTermCallSet r) vars

makeMatrixPair ::
  [PreTerm] -> [PreTerm] -> (Int, Int) -> (PreTerm, PreTerm)
makeMatrixPair params args (i, j) = (params !! (j-1), args !! (i-1))

makeCallMatrix :: Monad m =>
  Matrix (PreTerm, PreTerm) -> CallSetT m CallMatrix
makeCallMatrix = mapM (\(p, a) -> callRel p a)

callRel :: Monad m => PreTerm -> PreTerm -> CallSetT m CallRel
callRel p a = do
  b <- testEqual a
  --im0 <- lift (lift Env.getImplicitMap)
  --rm0 <- lift (lift Env.getRefMap)
  --let !() = trace ("param: " ++ preTermToString im0 rm0 0 p) ()
  --let !() = trace ("arg  : " ++ preTermToString im0 rm0 0 a) ()
  if b
  then return CallEqual
  else callRelNotEq p a
  where
    testEqual :: Monad m => PreTerm -> CallSetT m Bool
    testEqual b@(TermImplicitApp _ c _) = do
      (p', b') <- unifyTypes b
      rm <- lift (lift Env.getRefMap)
      let e = preTermsAlphaEqual rm p' b'
      fmap (e ||) (testEqual c)
    testEqual b@(TermApp _ c _) = do
      (p', b') <- unifyTypes b
      rm <- lift (lift Env.getRefMap)
      let e = preTermsAlphaEqual rm p' b'
      fmap (e ||) (testEqual c)
    testEqual b@(TermLazyApp _ c) = do
      (p', b') <- unifyTypes b
      rm <- lift (lift Env.getRefMap)
      let e = preTermsAlphaEqual rm p' b'
      fmap (e ||) (testEqual c)
    testEqual (TermLazyFun _ c) = testEqual c
    testEqual b = do
      (p', b') <- unifyTypes b
      rm <- lift (lift Env.getRefMap)
      return (preTermsAlphaEqual rm p' b')

    unifyTypes :: Monad m => PreTerm -> CallSetT m (PreTerm, PreTerm)
    unifyTypes b = do
      im <- lift (lift Env.getImplicitMap)
      rm <- lift (lift Env.getRefMap)
      case (preTermGetAlphaType im rm IntMap.empty p, preTermGetAlphaType im rm IntMap.empty b) of
        (Just pty, Just aty) -> do
          newids <- lift (lift Env.getNextVarId)
          un <- lift (lift (runExceptT (patternUnify2 newids pty aty)))
          case un of
            Left _ -> return (p, b)
            Right s -> return (substPreTerm s p, substPreTerm s b)
        _ -> return (p, b)

callRelNotEq :: Monad m => PreTerm -> PreTerm -> CallSetT m CallRel
callRelNotEq p a =
  case preTermProjArgs p of
    Nothing -> return CallUnknown
    Just (_, ps) -> callRelApp ps a

callRelApp :: Monad m => [PreTerm] -> PreTerm -> CallSetT m CallRel
callRelApp [] _ = return CallUnknown
callRelApp (p : ps) a = do
  r <- callRel p a
  case r of
    CallEqual -> return CallSmaller
    CallSmaller -> return CallSmaller
    CallUnknown -> callRelApp ps a

preTermAppCallSet :: Monad m =>
  Var -> SubstMap -> [PreTerm] -> CallSetT m (Set (VarId, CallMatrix))
preTermAppCallSet v rsu args = do
  rm <- lift (lift Env.getRefMap)
  --im <- lift (lift Env.getImplicitMap)
  case IntMap.lookup (varId v) rm of
    Nothing -> return Set.empty
    Just (r, _) -> do
      lift (pushCallStack v)
      let fs0 = IntSet.elems (preTermVars rm (termPre r))
      let fs = map (\i -> substPreTerm rsu (TermVar False (mkVar i "_"))) fs0
      let args' = fs ++ args
      if isNothing (IntMap.lookup (varId v) rm)
      then foldrM (\x s ->
                      fmap (Set.union s) (preTermCallSet x)
                  ) Set.empty args'
      else do
        as' <- ask
        let mp = Matrix.matrix (length args') (length as') (makeMatrixPair as' args')
        ma <- makeCallMatrix mp
        s0 <- foldrM (\x s ->
                      fmap (Set.union s) (preTermCallSet x)
                     ) Set.empty args'
        return (Set.insert (varId v, ma) s0)

preTermCallSet :: Monad m =>
  PreTerm -> CallSetT m (Set (VarId, CallMatrix))
preTermCallSet (TermRef v rsu) = do
  _ias <- lift (lift (Env.forceLookupImplicit (varId v)))
  let !() = assert (null _ias) ()
  rm <- lift (lift Env.getRefMap)
  case IntMap.lookup (varId v) rm of
   Nothing -> return Set.empty
   Just (r, _) -> do
    let (nis, nas) = case termPre r of
                      TermFun is _ Nothing _ -> (length is, 0)
                      TermFun is _ (Just n) _ -> (length is, n)
                      _ -> (0, 0)
    as <- lift (lift (makeArgVars [1..(nis + nas)]))
    preTermAppCallSet v rsu as
preTermCallSet (TermImplicitApp _ (TermRef v rsu) ias) = do
  rm <- lift (lift Env.getRefMap)
  case IntMap.lookup (varId v) rm of
    Nothing -> return Set.empty
    Just (r, _) -> do
      case termPre r of
        TermFun _ _ (Just n) _ -> do
          as <- lift (lift (makeArgVars [1..n]))
          preTermAppCallSet v rsu (map snd ias ++ as)
        _ -> preTermAppCallSet v rsu (map snd ias)
preTermCallSet (TermApp _ (TermImplicitApp _ (TermRef v rsu) ias) as) = do
  rm <- lift (lift Env.getRefMap)
  case IntMap.lookup (varId v) rm of
    Nothing -> return Set.empty
    Just (r, _) -> do
      case termPre r of
        TermFun _ _ (Just _) _ ->
          preTermAppCallSet v rsu (map snd ias ++ as)
        _ -> do
          s0 <- preTermAppCallSet v rsu (map snd ias)
          s1 <- foldrM (\x s ->
                          fmap (Set.union s) (preTermCallSet x)
                       ) Set.empty as
          return (Set.union s0 s1)
preTermCallSet (TermApp _ (TermRef v rsu) as) = do
  _ias <- lift (lift (Env.forceLookupImplicit (varId v)))
  let !() = assert (null _ias) ()
  rm <- lift (lift Env.getRefMap)
  case IntMap.lookup (varId v) rm of
    Nothing -> return Set.empty
    Just (r, _) -> do
      case termPre r of
        TermFun _ _ (Just _) _ ->
          preTermAppCallSet v rsu as
        _ -> do
          s0 <- preTermAppCallSet v rsu []
          s1 <- foldrM (\x s ->
                          fmap (Set.union s) (preTermCallSet x)
                       ) Set.empty as
          return (Set.union s0 s1)
preTermCallSet (TermApp _ a as) = do
  s0 <- foldrM (\x s ->
                  fmap (Set.union s) (preTermCallSet x)
               ) Set.empty as
  s1 <- preTermCallSet a
  return (Set.union s0 s1)
preTermCallSet (TermImplicitApp _ a as) = do
  s0 <- foldrM (\(_, x) s ->
                  fmap (Set.union s) (preTermCallSet x)
               ) Set.empty as
  s1 <- preTermCallSet a
  return (Set.union s0 s1)
preTermCallSet (TermFun _ _ _ ct) = caseTreeLeafsCallSet ct
preTermCallSet (TermLazyFun _ x) = preTermCallSet x
preTermCallSet (TermArrow _ d c) = do
  s0 <- foldrM (\(_, x) a ->
                  fmap (Set.union a) (preTermCallSet x)
               ) Set.empty d
  s1 <- preTermCallSet c
  return (Set.union s0 s1)
preTermCallSet (TermLazyArrow _ x) = preTermCallSet x
preTermCallSet (TermLazyApp _ x) = preTermCallSet x
preTermCallSet (TermMatch x ct) = do
  s0 <- preTermCallSet x
  s1 <- caseTreeCallSet ct [x]
  return (Set.union s0 s1)
preTermCallSet (TermVar _ _) = return Set.empty
preTermCallSet (TermData _) = return Set.empty
preTermCallSet (TermCtor _ _) = return Set.empty
preTermCallSet TermUnitElem = return Set.empty
preTermCallSet TermUnitTy = return Set.empty
preTermCallSet TermTy = return Set.empty
preTermCallSet TermEmpty = return Set.empty

caseTreeLeafsCallSet :: Monad m =>
  CaseTree -> CallSetT m (Set (VarId, CallMatrix))
caseTreeLeafsCallSet (CaseEmpty _) = return Set.empty
caseTreeLeafsCallSet (CaseUnit _ (_, ct)) = caseTreeLeafsCallSet ct
caseTreeLeafsCallSet (CaseLeaf _ _ x _) = preTermCallSet x
caseTreeLeafsCallSet (CaseNode _ m d) = do
  s0 <- foldrM (\(_, x) a ->
                  fmap (Set.union a) (caseTreeLeafsCallSet x)
               ) Set.empty (IntMap.elems m)
  s1 <- case d of
          Nothing -> return Set.empty
          Just (_, d') -> caseTreeLeafsCallSet d'
  return (Set.union s0 s1)

runCaseTreeCallSet :: Monad m =>
  CaseTree -> [PreTerm] -> [PreTerm] ->
  CallMapT m (Set (VarId, CallMatrix))
runCaseTreeCallSet ct vars as =
  runReaderT (caseTreeCallSet ct as) (vars ++ as)

caseTreeCallSet :: Monad m =>
  CaseTree -> [PreTerm] -> CallSetT m (Set (VarId, CallMatrix))
caseTreeCallSet (CaseEmpty _) _ = return Set.empty
caseTreeCallSet (CaseUnit idx a) xs = do
  let (x, xs') = listRemoveIdx idx xs
  caseTreeCatchAllCallSet x xs' (Just a)
caseTreeCallSet (CaseLeaf is _ te _) xs = do
  let s0 = assert (length xs == length is) (zip (map varId is) xs)
  newids <- lift (lift Env.getNextVarId)
  s <- foldlM (\a (i,t) ->
        lift . lift $ tryAddSubst newids i t a) IntMap.empty s0
  let te' = substPreTerm s te
  local (newEnvCallSet s) (preTermCallSet te')
  where
    tryAddSubst :: Monad m =>
      VarId -> VarId -> PreTerm -> SubstMap -> TypeCheckT m SubstMap
    tryAddSubst newids i t a = do
      tcMergePatUnifMaps newids (loc 0 0) a (IntMap.singleton i t)
        `catchError` (\_ -> return a)
caseTreeCallSet (CaseNode idx m d) xs = do
  let (x, xs') = listRemoveIdx idx xs
  s0 <- foldrM (caseTreeAddCase x xs') Set.empty (IntMap.toList m)
  s1 <- caseTreeCatchAllCallSet x xs' d
  return (Set.union s0 s1)

removeLazyApps :: PreTerm -> PreTerm
removeLazyApps (TermLazyApp _ t) = removeLazyApps t
removeLazyApps t = t

caseTreeAddCase :: Monad m =>
  PreTerm -> [PreTerm] -> (VarId, ([Var], CaseTree)) ->
  Set (VarId, CallMatrix) -> CallSetT m (Set (VarId, CallMatrix))
caseTreeAddCase x0 xs (cid, (is, ct)) acc = do
  cr <- lift (lift (Env.forceLookupRef cid))
  case termPre cr of
    TermCtor v i -> do
      cpa <- lift (lift (prePatternApplyWithVars (PatternCtor v i) (termTy cr)))
      let cte = prePatternToPreTerm (patternPre cpa)
      let (_, as) = fromJust (preTermProjArgs cte)
      let xs' = as ++ xs
      newids <- lift (lift Env.getNextVarId)
      --im <- lift (lift Env.getImplicitMap)
      --rm <- lift (lift Env.getRefMap)
      --let !() = trace ("unify " ++ preTermToString im rm 0 cte ++ " with " ++ preTermToString im rm 0 x) ()
      let x = removeLazyApps x0
      unif <- lift (lift (runExceptT (patternUnify2NoNormalize newids cte x)))
      case unif of
        Right s -> do
          --let !() = trace ("unify subst map: " ++ show s) ()
          let ct' = substCaseTree s ct
          local (newEnvCallSet s) $
            fmap (Set.union acc) (caseTreeCatchAllCallSet x xs' (Just (is, ct')))
        Left (UnifyAbsurd _) -> return acc
        Left (UnifyUnable _) ->
          fmap (Set.union acc) (caseTreeCatchAllCallSet x xs' (Just (is, ct)))
    _ -> error ("expected var id " ++ show cid ++ " to be a ctor")

caseTreeCatchAllCallSet :: Monad m =>
  PreTerm -> [PreTerm] -> Maybe ([Var], CaseTree) ->
  CallSetT m (Set (VarId, CallMatrix))
caseTreeCatchAllCallSet _ _ Nothing = return Set.empty
caseTreeCatchAllCallSet x xs' (Just (is, ct)) = do
  let s0 = zip (map varId is) (repeat x)
  let s = IntMap.fromList s0
  let ct' = substCaseTree s ct
  local (newEnvCallSet s) (caseTreeCallSet ct' xs')

newEnvCallSet :: SubstMap -> [PreTerm] -> [PreTerm]
newEnvCallSet s as = map (substPreTerm s) as

listRemoveIdx :: Int -> [a] -> (a, [a])
listRemoveIdx 0 (x:xs) = (x, xs)
listRemoveIdx i (x:xs) =
  let (x', xs') = listRemoveIdx (i - 1) xs in (x', x : xs')
listRemoveIdx _ _ = error "incompatible listRemoveIdx index with list"

----------------------------------------------------------------------

verifyRecursiveImpureChains :: Monad m => VarId -> TypeCheckT m ()
verifyRecursiveImpureChains i = do
  meta <- Env.forceLookupRefMeta i
  when (not (refMetaIsDeclaredPure meta)
          && not (refMetaIsTerminationChecked meta))
    (preTermOfRef >>= verifyRecursiveRefsAreImpure i)
  where
    preTermOfRef :: Monad m => TypeCheckT m PreTerm
    preTermOfRef = fmap termPre (Env.forceLookupRef i)

verifyImpureIsNotTerminationChecked :: Monad m => VarId -> TypeCheckT m ()
verifyImpureIsNotTerminationChecked i = do
  meta <- Env.forceLookupRefMeta i
  isData <- Env.isDataType i
  if isData
  then when (not (refMetaIsDeclaredPure meta)
            && refMetaIsTerminationChecked meta) $ do
        err (refMetaLoc meta)
          (Fatal $
            quote (refMetaName meta) ++ " declared with with "
             ++ quote ("data.. " ++ refMetaName meta)
             ++ ", but can be declared with "
             ++ quote ("data " ++ refMetaName meta)
             ++ ", since it passed strict positivity check")
  else when (not (refMetaIsDeclaredPure meta)
            && refMetaIsTerminationChecked meta) $ do
        err (refMetaLoc meta)
          (Fatal $
            quote (refMetaName meta) ++ " declared with with "
             ++ quote ("val.. " ++ refMetaName meta)
             ++ ", but can be declared with "
             ++ quote ("val " ++ refMetaName meta)
             ++ ", since it passed termination check")

directlyAccessibleRefs :: PreTerm -> VarIdSet
directlyAccessibleRefs (TermFun _ _ _ caseTree) =
  caseTreeDirectlyAccessibleRefs caseTree
directlyAccessibleRefs (TermLazyFun _ t) = directlyAccessibleRefs t
directlyAccessibleRefs (TermArrow _ as t) =
  let f s (_, i) = IntSet.union s (directlyAccessibleRefs i)
      as' = foldl f IntSet.empty as
      a' = directlyAccessibleRefs t
  in IntSet.union a' as'
directlyAccessibleRefs (TermLazyArrow _ t) = directlyAccessibleRefs t
directlyAccessibleRefs (TermApp _ a as) =
  let a' = directlyAccessibleRefs a
      f s i = IntSet.union s (directlyAccessibleRefs i)
      as' = foldl f IntSet.empty as
  in IntSet.union a' as'
directlyAccessibleRefs (TermImplicitApp _ a as) =
  let a' = directlyAccessibleRefs a
      f s i = IntSet.union s (directlyAccessibleRefs (snd i))
      as' = foldl f IntSet.empty as
  in IntSet.union a' as'
directlyAccessibleRefs (TermLazyApp _ t) = directlyAccessibleRefs t
directlyAccessibleRefs (TermRef v _) = IntSet.singleton (varId v)
directlyAccessibleRefs (TermVar _ _) = IntSet.empty
directlyAccessibleRefs (TermData _) = IntSet.empty
directlyAccessibleRefs (TermCtor _ _) = IntSet.empty
directlyAccessibleRefs (TermMatch t ct) =
  let t' = directlyAccessibleRefs t
      ct' = caseTreeDirectlyAccessibleRefs ct
  in IntSet.union t' ct'
directlyAccessibleRefs TermUnitElem = IntSet.empty
directlyAccessibleRefs TermUnitTy = IntSet.empty
directlyAccessibleRefs TermEmpty = IntSet.empty
directlyAccessibleRefs TermTy = IntSet.empty

caseTreeDirectlyAccessibleRefs :: CaseTree -> VarIdSet
caseTreeDirectlyAccessibleRefs = caseRefs
  where
    caseRefs :: CaseTree -> VarIdSet
    caseRefs (CaseLeaf _ _ t _) = directlyAccessibleRefs t
    caseRefs (CaseEmpty _) = IntSet.empty
    caseRefs (CaseNode _ m d) =
      let i1 = foldl addCaseTree IntSet.empty m
      in case d of
          Nothing -> i1
          Just d' -> IntSet.union i1 (addCaseTree IntSet.empty d')
    caseRefs (CaseUnit _ d) = addCaseTree IntSet.empty d

    addCaseTree :: VarIdSet -> ([Var], CaseTree) -> VarIdSet
    addCaseTree s (_, t) = IntSet.union (caseRefs t) s

verifyRecursiveRefsAreImpure :: Monad m => VarId -> PreTerm -> TypeCheckT m ()
verifyRecursiveRefsAreImpure v t = do
  rm <- Env.getRefMap
  let rs = IntSet.delete v (directlyAccessibleRefs t)
  rs' <- mapM (\r ->
            fmap (\x -> (r, termPre x)) (Env.forceLookupRef r)
          ) (IntSet.elems rs)
  let ds = map (\(r1, r2) -> (r1, preTermRefs rm r2)) rs'
  let ds' = filter (\(_, x) -> IntSet.member v x) ds
  meta <- Env.forceLookupRefMeta v
  mapM_ (verifyImpure (refMetaName meta) . fst) ds'
  where
    verifyImpure :: Monad m => VarName -> VarId -> TypeCheckT m ()
    verifyImpure na d = do
      meta <- Env.forceLookupRefMeta d
      when (refMetaIsDeclaredPure meta)
        (err (refMetaLoc meta) (Fatal $
            "expected " ++ quote (refMetaName meta)
            ++ " to be declared with "
            ++ quote ("val.. " ++ refMetaName meta)
            ++ ", because of mutual definition with " ++ quote na))
      let meta' = meta {refMetaIsTerminationChecked = False}
      Env.forceInsertRefMeta d meta'

----------------------------------------------------------------------

verifyDataImpureChains :: Monad m => RefVar -> TypeCheckT m ()
verifyDataImpureChains (RefData v) = do
  meta <- Env.forceLookupRefMeta (varId v)
  when (not (refMetaIsDeclaredPure meta)
          && not (refMetaIsTerminationChecked meta))
    (ctorsTy >>= mapM_ (verifyRecursiveDataAreImpure (varId v)))
  where
    ctorsTy :: Monad m => TypeCheckT m [PreTerm]
    ctorsTy = do
      cs <- Env.forceLookupDataCtor (varId v)
      mapM (fmap termTy . Env.forceLookupRef . varId) cs
verifyDataImpureChains _ = return ()

accessibleData :: RefMap -> IntSet -> PreTerm -> VarIdSet
accessibleData rm vis (TermFun _ _ _ caseTree) =
  caseTreeDirectlyAccessibleData rm vis caseTree
accessibleData rm vis (TermLazyFun _ t) = accessibleData rm vis t
accessibleData rm vis (TermArrow _ as t) =
  let f s (_, i) = IntSet.union s (accessibleData rm vis i)
      as' = foldl f IntSet.empty as
      a' = accessibleData rm vis t
  in IntSet.union a' as'
accessibleData rm vis (TermLazyArrow _ t) = accessibleData rm vis t
accessibleData rm vis (TermApp _ a as) =
  let a' = accessibleData rm vis a
      f s i = IntSet.union s (accessibleData rm vis i)
      as' = foldl f IntSet.empty as
  in IntSet.union a' as'
accessibleData rm vis (TermImplicitApp _ a as) =
  let a' = accessibleData rm vis a
      f s i = IntSet.union s (accessibleData rm vis (snd i))
      as' = foldl f IntSet.empty as
  in IntSet.union a' as'
accessibleData rm vis (TermLazyApp _ t) = accessibleData rm vis t
accessibleData rm vis (TermRef v s) = 
  if IntSet.member (varId v) vis
  then IntSet.empty
  else
    let vis' = IntSet.insert (varId v) vis
        r = Env.forceLookupRefMap (varId v) rm
    in accessibleData rm vis' (substPreTerm s (termPre r))
accessibleData _ _ (TermVar _ _) = IntSet.empty
accessibleData _ _ (TermData v) = IntSet.singleton (varId v)
accessibleData _ _ (TermCtor _ _) = IntSet.empty
accessibleData rm vis (TermMatch t ct) =
  let t' = accessibleData rm vis t
      ct' = caseTreeDirectlyAccessibleData rm vis ct
  in IntSet.union t' ct'
accessibleData _ _ TermUnitElem = IntSet.empty
accessibleData _ _ TermUnitTy = IntSet.empty
accessibleData _ _ TermEmpty = IntSet.empty
accessibleData _ _ TermTy = IntSet.empty

caseTreeDirectlyAccessibleData :: RefMap -> IntSet -> CaseTree -> VarIdSet
caseTreeDirectlyAccessibleData rm vis = castData
  where
    castData :: CaseTree -> VarIdSet
    castData (CaseLeaf _ _ t _) = accessibleData rm vis t
    castData (CaseEmpty _) = IntSet.empty
    castData (CaseNode _ m d) =
      let i1 = foldl addCaseTree IntSet.empty m
      in case d of
          Nothing -> i1
          Just d' -> IntSet.union i1 (addCaseTree IntSet.empty d')
    castData (CaseUnit _ d) = addCaseTree IntSet.empty d

    addCaseTree :: VarIdSet -> ([Var], CaseTree) -> VarIdSet
    addCaseTree s (_, t) = IntSet.union (castData t) s

verifyRecursiveDataAreImpure :: Monad m => VarId -> PreTerm -> TypeCheckT m ()
verifyRecursiveDataAreImpure dv ct = do
  meta <- Env.forceLookupRefMeta dv
  if refMetaIsDeclaredPure meta
  then return ()
  else do
    rm <- Env.getRefMap
    let rs = accessibleData rm IntSet.empty ct
    cs <- mapM (\r -> do
              ctors <- Env.forceLookupDataCtor r
              ctors' <- mapM (fmap termTy . Env.forceLookupRef . varId) ctors
              return (r, ctors')
            ) (IntSet.elems rs)
    mapM_ (doVerify (refMetaName meta)) cs
  where
    doVerify :: Monad m => VarName -> (VarId, [PreTerm]) -> TypeCheckT m ()
    doVerify dna (cv, ts) = mapM_ (doVerify' dna cv) ts

    doVerify' :: Monad m => VarName -> VarId -> PreTerm -> TypeCheckT m ()
    doVerify' dna cv t = do
      meta <- Env.forceLookupRefMeta cv
      b <- positivityCheckContainsData IntSet.empty (mkVar dv dna) t
      when (b && refMetaIsDeclaredPure meta)
        (err (refMetaLoc meta) (Fatal $
            "expected " ++ quote (refMetaName meta)
            ++ " to be declared with "
            ++ quote ("data.. " ++ refMetaName meta)
            ++ ", because of mutual definition with " ++ quote dna))
      when b $ do
        let meta' = meta {refMetaIsTerminationChecked = False}
        Env.forceInsertRefMeta cv meta'

----------------------------------------------------------------------

strictPositivityCheck :: Monad m =>
  Loc -> Var -> Implicits -> PreTerm -> TypeCheckT m ()
strictPositivityCheck lo dv imps ty = do
  let ty' = TermArrow False (map (\(v, t) -> (Just v, t)) imps) ty
  meta <- Env.forceLookupRefMeta (varId dv)
  let meta' = meta {refMetaIsTerminationChecked = True}
  Env.forceInsertRefMeta (varId dv) meta'
  rm <- Env.getRefMap
  positivityCheckCod lo dv (preTermNormalize rm ty')

positivityFail :: Monad m =>
  Loc -> Var -> Maybe String -> TypeCheckT m ()
positivityFail lo dv msg = do
  let na = varName dv
  meta <- Env.forceLookupRefMeta (varId dv)
  let meta' = meta {refMetaIsTerminationChecked = False}
  Env.forceInsertRefMeta (varId dv) meta'
  let postfix = case msg of
                  Nothing -> ""
                  Just s -> ", " ++ s
  when (refMetaIsDeclaredPure meta) $ do
    err lo (Fatal $
      "strict positivity check for " ++ quote na ++ " failed"
        ++ postfix
        ++ ", declare with " ++ quote ("data.. " ++ na)
        ++ " to disable this check")

positivityCheckCod :: Monad m =>
  Loc -> Var -> PreTerm -> TypeCheckT m ()
positivityCheckCod lo dv (TermFun _ _ _ ct) =
  positivityCheckDomCaseTree lo IntSet.empty dv ct
positivityCheckCod lo dv (TermLazyFun _ t) =
  positivityCheckDom lo IntSet.empty dv t
positivityCheckCod lo dv (TermArrow _ d c) = do
  mapM_ checkDom d
  positivityCheckCod lo dv c
  where
    checkDom :: Monad m => (Maybe Var, PreTerm) -> TypeCheckT m ()
    checkDom (Nothing, t) = positivityCheckDom lo IntSet.empty dv t
    checkDom (Just v, t) = do
      b <- positivityCheckContainsData IntSet.empty dv t
      when b (positivityFail lo dv
        (Just ("argument " ++ quote (varName v) ++ " cannot be dependent")))
      positivityCheckDom lo IntSet.empty dv t
positivityCheckCod lo dv (TermLazyArrow _ t) = positivityCheckCod lo dv t
positivityCheckCod lo dv (TermApp _ f xs) = do
  c <- foldlM (\b x ->
          fmap (b ||) (positivityCheckContainsData IntSet.empty dv x)
        ) False xs
  when c (positivityCheckVerifySimple lo dv f)
  mapM_ (\x -> positivityCheckDom lo IntSet.empty dv x) xs
  positivityCheckDom lo IntSet.empty dv f
positivityCheckCod lo dv (TermImplicitApp _ f xs) = do
  c <- foldlM (\b (_, x) ->
          fmap (b ||) (positivityCheckContainsData IntSet.empty dv x)
        ) False xs
  when c (positivityCheckVerifySimple lo dv f)
  mapM_ (\(_, x) -> positivityCheckDom lo IntSet.empty dv x) xs
  positivityCheckDom lo IntSet.empty dv f
positivityCheckCod lo dv (TermLazyApp _ t) =
  positivityCheckDom lo IntSet.empty dv t
positivityCheckCod lo dv (TermRef v s) =
  positivityCheckDom lo IntSet.empty dv (TermRef v s)
positivityCheckCod _ _ (TermVar _ _) = return ()
positivityCheckCod _ _ (TermData _) = return ()
positivityCheckCod _ _ (TermCtor _ _) = return ()
positivityCheckCod lo dv (TermMatch t ct) = do
  positivityCheckDom lo IntSet.empty dv t
  positivityCheckDomCaseTree lo IntSet.empty dv ct
positivityCheckCod _ _ TermUnitElem = return ()
positivityCheckCod _ _ TermUnitTy = return ()
positivityCheckCod _ _ TermTy = return ()
positivityCheckCod _ _ TermEmpty = return ()

positivityCheckVerifySimple :: Monad m =>
  Loc -> Var -> PreTerm -> TypeCheckT m ()
positivityCheckVerifySimple lo dv (TermFun _ _ _ _) = positivityFail lo dv Nothing
positivityCheckVerifySimple lo dv (TermRef _ _) = positivityFail lo dv Nothing
positivityCheckVerifySimple lo dv (TermArrow _ _ _) = positivityFail lo dv Nothing
positivityCheckVerifySimple lo dv (TermLazyArrow _ _) = positivityFail lo dv Nothing
positivityCheckVerifySimple lo dv (TermMatch _ _) = positivityFail lo dv Nothing
positivityCheckVerifySimple lo dv (TermLazyFun _ t) =
  positivityCheckVerifySimple lo dv t
positivityCheckVerifySimple lo dv (TermApp _ f _) =
  positivityCheckVerifySimple lo dv f
positivityCheckVerifySimple lo dv (TermImplicitApp _ f _) =
  positivityCheckVerifySimple lo dv f
positivityCheckVerifySimple lo dv (TermLazyApp _ t) =
  positivityCheckVerifySimple lo dv t
positivityCheckVerifySimple _ _ (TermVar _ _) = return ()
positivityCheckVerifySimple _ _ (TermData _) = return ()
positivityCheckVerifySimple _ _ (TermCtor _ _) = return ()
positivityCheckVerifySimple _ _ TermUnitElem = return ()
positivityCheckVerifySimple _ _ TermUnitTy = return ()
positivityCheckVerifySimple _ _ TermTy = return ()
positivityCheckVerifySimple _ _ TermEmpty = return ()

positivityCheckContainsData :: Monad m =>
  IntSet -> Var -> PreTerm -> TypeCheckT m Bool
positivityCheckContainsData vi dv (TermFun _ _ _ ct) =
  positivityCheckContainsDataCaseTree vi dv ct
positivityCheckContainsData vi dv (TermLazyFun _ t) =
  positivityCheckContainsData vi dv t
positivityCheckContainsData vi dv (TermArrow _ d c) = do
  b1 <- foldlM addArg False d
  b2 <- positivityCheckContainsData vi dv c
  return (b1 || b2)
  where
    addArg :: Monad m => Bool -> (a, PreTerm) -> TypeCheckT m Bool
    addArg True _ = return True
    addArg False (_, t) = positivityCheckContainsData vi dv t
positivityCheckContainsData vi dv (TermLazyArrow _ t) =
  positivityCheckContainsData vi dv t
positivityCheckContainsData vi dv (TermApp _ f xs) = do
  b1 <- foldlM addArg False xs
  b2 <- positivityCheckContainsData vi dv f
  return (b1 || b2)
  where
    addArg :: Monad m => Bool -> PreTerm -> TypeCheckT m Bool
    addArg True _ = return True
    addArg False t = positivityCheckContainsData vi dv t
positivityCheckContainsData vi dv (TermImplicitApp _ f xs) = do
  b1 <- foldlM addArg False xs
  b2 <- positivityCheckContainsData vi dv f
  return (b1 || b2)
  where
    addArg :: Monad m => Bool -> (a, PreTerm) -> TypeCheckT m Bool
    addArg True _ = return True
    addArg False (_, t) = positivityCheckContainsData vi dv t
positivityCheckContainsData vi dv (TermLazyApp _ t) =
  positivityCheckContainsData vi dv t
positivityCheckContainsData vi dv (TermRef v s) = do
  if IntSet.member (varId v) vi
  then return False
  else do
    let vi' = IntSet.insert (varId v) vi
    r <- Env.forceLookupRef (varId v)
    positivityCheckContainsData vi' dv (substPreTerm s (termPre r))
positivityCheckContainsData _ _ (TermVar _ _) = return False
positivityCheckContainsData vi dv (TermData v)
  | varId dv == varId v = return True
  | True =
    if IntSet.member (varId v) vi
    then return False
    else do
      let vi' = IntSet.insert (varId v) vi
      d <- Env.forceLookupRef (varId v)
      b1 <- positivityCheckContainsData vi' dv (termTy d)
      cs <- Env.forceLookupDataCtor (varId v)
      b2 <- foldlM (addDataCtor vi') False cs
      return (b1 || b2)
      where
        addDataCtor :: Monad m => IntSet -> Bool -> Var -> TypeCheckT m Bool
        addDataCtor vi' a c = do
          r <- Env.forceLookupRef (varId c)
          b <- positivityCheckContainsData vi' dv (termTy r)
          return (a || b)
positivityCheckContainsData _ _ (TermCtor _ _) = return False
positivityCheckContainsData vi dv (TermMatch t ct) = do
  b1 <- positivityCheckContainsData vi dv t
  b2 <- positivityCheckContainsDataCaseTree vi dv ct
  return (b1 || b2)
positivityCheckContainsData _ _ TermUnitElem = return False
positivityCheckContainsData _ _ TermUnitTy = return False
positivityCheckContainsData _ _ TermTy = return False
positivityCheckContainsData _ _ TermEmpty = return False

positivityCheckContainsDataCaseTree :: Monad m =>
  IntSet -> Var -> CaseTree -> TypeCheckT m Bool
positivityCheckContainsDataCaseTree _ _ (CaseEmpty _) = return False
positivityCheckContainsDataCaseTree vi dv (CaseUnit _ (_, ct)) =
  positivityCheckContainsDataCaseTree vi dv ct
positivityCheckContainsDataCaseTree vi dv (CaseLeaf _ _ te _) =
  positivityCheckContainsData vi dv te
positivityCheckContainsDataCaseTree vi dv (CaseNode _ m d) = do
  b <- foldlM (\a (_, t) ->
            fmap (a || ) (positivityCheckContainsDataCaseTree vi dv t)
          ) False (IntMap.elems m)
  case d of
    Nothing -> return b
    Just (_, d') ->
      fmap (b ||) (positivityCheckContainsDataCaseTree vi dv d')

positivityCheckDom :: Monad m =>
  Loc -> IntSet -> Var -> PreTerm -> TypeCheckT m ()
positivityCheckDom lo vi dv (TermFun _ _ _ ct) =
  positivityCheckDomCaseTree lo vi dv ct
positivityCheckDom lo vi dv (TermLazyFun _ t) = positivityCheckDom lo vi dv t
positivityCheckDom lo vi dv (TermArrow _ d c) = do
  mapM_ checkDom d
  positivityCheckDom lo vi dv c
  where
    checkDom :: Monad m => (Maybe Var, PreTerm) -> TypeCheckT m ()
    checkDom (_, t) = do
      b <- positivityCheckContainsData IntSet.empty dv t
      when b (positivityFail lo dv Nothing)
positivityCheckDom lo vi dv (TermLazyArrow _ t) = positivityCheckDom lo vi dv t
positivityCheckDom lo vi dv (TermApp _ f xs) = do
  c <- foldlM (\b x ->
          fmap (b ||) (positivityCheckContainsData IntSet.empty dv x)
        ) False xs
  when c (positivityCheckVerifySimple lo dv f)
  mapM_ (\x -> positivityCheckDom lo vi dv x) xs
  positivityCheckDom lo vi dv f
positivityCheckDom lo vi dv (TermImplicitApp _ f xs) = do
  c <- foldlM (\b (_, x) ->
            fmap (b ||) (positivityCheckContainsData IntSet.empty dv x)
          ) False xs
  when c (positivityCheckVerifySimple lo dv f)
  mapM_ (\(_, x) -> positivityCheckDom lo vi dv x) xs
  positivityCheckDom lo vi dv f
positivityCheckDom lo vi dv (TermLazyApp _ t) = positivityCheckDom lo vi dv t
positivityCheckDom lo vi dv (TermRef v s) =
  if IntSet.member (varId v) vi
  then return ()
  else do
    let vi' = IntSet.insert (varId v) vi
    r <- Env.forceLookupRef (varId v)
    positivityCheckDom lo vi' dv (substPreTerm s (termPre r))
positivityCheckDom _ _ _ (TermVar _ _) = return ()
positivityCheckDom _ _ _ (TermData _) = return ()
positivityCheckDom _ _ _ (TermCtor _ _) = return ()
positivityCheckDom lo vi dv (TermMatch t ct) = do
  positivityCheckDom lo vi dv t
  positivityCheckDomCaseTree lo vi dv ct
positivityCheckDom _ _ _ TermUnitElem = return ()
positivityCheckDom _ _ _ TermUnitTy = return ()
positivityCheckDom _ _ _ TermTy = return ()
positivityCheckDom _ _ _ TermEmpty = return ()

positivityCheckDomCaseTree :: Monad m =>
  Loc -> IntSet -> Var -> CaseTree -> TypeCheckT m ()
positivityCheckDomCaseTree _ _ _ (CaseEmpty _) = return ()
positivityCheckDomCaseTree lo vi dv (CaseUnit _ (_, ct)) =
  positivityCheckDomCaseTree lo vi dv ct
positivityCheckDomCaseTree lo vi dv (CaseLeaf _ _ te _) =
  positivityCheckDom lo vi dv te
positivityCheckDomCaseTree lo vi dv (CaseNode _ m d) = do
  mapM_ (\(_, t) -> positivityCheckDomCaseTree lo vi dv t) (IntMap.elems m)
  case d of
    Nothing -> return ()
    Just (_, d') -> positivityCheckDomCaseTree lo vi dv d'
