{-# LANGUAGE BangPatterns #-}

module TypeCheck.TermEnv
  ( preTermRefs
  , preTermExistsVar
  , prePatternVars
  , preTermCodRootType
  , preTermsEqual
  , preTermListsEqual
  , preTermsAlphaEqual
  , preTermNormalize
  , preTermFinalCod
  , preTermDomCod
  , preTermLazyCod
  , preTermLazyOrDelayCod
  , preTermPartialApplication
  , patternProjArgs
  , patternApply
  , prePatternApplyWithVars
  , preTermProjArgs
  , preTermToString
  , prePatternToString
  , preTermGetAlphaType
  , caseTreeToString
  , getAppAlphaArgumentWeight
  )
where

import TypeCheck.TypeCheckIO
import TypeCheck.SubstMap
import TypeCheck.Term
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified TypeCheck.Env as Env
import Data.Maybe (fromJust, isJust, isNothing)
import Control.Exception (assert)
import Control.Monad.Trans.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative ((<|>))
--import Data.List (sortOn, intercalate)
import Data.List (sortOn, sortBy, findIndex)
import qualified Str

import Debug.Trace (trace)

_useTrace :: String -> a -> a
_useTrace = trace

prePatternVars :: PrePattern -> VarIdSet
prePatternVars (PatternVar v) = IntSet.singleton (varId v)
prePatternVars (PatternLazyApp p) = prePatternVars p
prePatternVars (PatternApp p ps) =
  let p' = prePatternVars p
      ps' = foldl IntSet.union IntSet.empty (map prePatternVars ps)
  in IntSet.union p' ps'
prePatternVars (PatternImplicitApp _ p ps) =
  let p' = prePatternVars p
      ps' = foldl IntSet.union IntSet.empty (map (prePatternVars . snd) ps)
  in IntSet.union p' ps'
prePatternVars (PatternCtor _ _) = IntSet.empty
prePatternVars PatternUnit = IntSet.empty
prePatternVars PatternEmpty = IntSet.empty

caseTreeRefs :: IntSet -> RefMap -> CaseTree -> VarIdSet
caseTreeRefs vis rm = caseRefs
  where
    caseRefs :: CaseTree -> VarIdSet
    caseRefs (CaseLeaf _ t _) = doPreTermRefs vis rm t
    caseRefs (CaseEmpty _) = IntSet.empty
    caseRefs (CaseNode _ m d) =
      let i1 = foldl addCaseTree IntSet.empty m
      in case d of
          Nothing -> i1
          Just d' -> IntSet.union i1 (addCaseTree IntSet.empty d')
    caseRefs (CaseUnit _ d) = addCaseTree IntSet.empty d

    addCaseTree :: VarIdSet -> ([Var], CaseTree) -> VarIdSet
    addCaseTree s (_, t) = IntSet.union (caseRefs t) s

preTermRefs :: RefMap -> PreTerm -> VarIdSet
preTermRefs r t = doPreTermRefs IntSet.empty r t

doPreTermRefs :: IntSet -> RefMap -> PreTerm -> VarIdSet
doPreTermRefs vis r (TermFun _ _ _ caseTree) = caseTreeRefs vis r caseTree
doPreTermRefs vis r (TermLazyFun _ t) = doPreTermRefs vis r t
doPreTermRefs vis r (TermArrow _ as t) =
  let f s (_, i) = IntSet.union s (doPreTermRefs vis r i)
      as' = foldl f IntSet.empty as
      a' = doPreTermRefs vis r t
  in IntSet.union a' as'
doPreTermRefs vis r (TermLazyArrow _ t) = doPreTermRefs vis r t
doPreTermRefs vis r (TermApp _ a as) =
  let a' = doPreTermRefs vis r a
      f s i = IntSet.union s (doPreTermRefs vis r i)
      as' = foldl f IntSet.empty as
  in IntSet.union a' as'
doPreTermRefs vis r (TermImplicitApp _ a as) =
  let a' = doPreTermRefs vis r a
      f s i = IntSet.union s (doPreTermRefs vis r (snd i))
      as' = foldl f IntSet.empty as
  in IntSet.union a' as'
doPreTermRefs vis r (TermLazyApp _ t) = doPreTermRefs vis r t
doPreTermRefs vis r (TermRef v su) = do
  if IntSet.member (varId v) vis
    then IntSet.empty
    else let vis' = IntSet.insert (varId v) vis
             t0 = IntMap.lookup (varId v) r
         in case t0 of
              Nothing -> IntSet.singleton (varId v)
              Just (t, _) ->
                let s = doPreTermRefs vis' r (substPreTerm r su (termPre t))
                in IntSet.insert (varId v) s
doPreTermRefs _ _ (TermVar _ _) = IntSet.empty
doPreTermRefs _ _ (TermData _) = IntSet.empty
doPreTermRefs _ _ (TermCtor _ _) = IntSet.empty
doPreTermRefs vis r (TermCase t ct) =
  let t' = doPreTermRefs vis r t
      ct' = caseTreeRefs vis r ct
  in IntSet.union t' ct'
doPreTermRefs _ _ TermUnitElem = IntSet.empty
doPreTermRefs _ _ TermUnitTy = IntSet.empty
doPreTermRefs _ _ TermEmpty = IntSet.empty
doPreTermRefs _ _ TermTy = IntSet.empty

preTermsEqual :: RefMap -> PreTerm -> PreTerm -> Bool
preTermsEqual rm t1 t2 =
  preTermsAlphaEqual rm (preTermNormalize rm t1) (preTermNormalize rm t2)

preTermListsEqual :: RefMap -> [PreTerm] -> [PreTerm] -> Bool
preTermListsEqual _ [] [] = True
preTermListsEqual _ (_:_) [] = False
preTermListsEqual _ [] (_:_) = False
preTermListsEqual rm (p:ps) (q:qs) =
  preTermsEqual rm p q && preTermListsEqual rm ps qs

preTermsAlphaEqual :: RefMap -> PreTerm -> PreTerm -> Bool
preTermsAlphaEqual = doPreTermsAlphaEqual []

type RefBisim = [((Var, SubstMap), (Var, SubstMap))]

-- The case for TermRef is dependent on the argument order is kept
-- in recursive calls.
doPreTermsAlphaEqual :: RefBisim -> RefMap -> PreTerm -> PreTerm -> Bool
doPreTermsAlphaEqual _ _ (TermVar _ v1) (TermVar _ v2) = varId v1 == varId v2
doPreTermsAlphaEqual _ _ (TermData v1) (TermData v2) = varId v1 == varId v2
doPreTermsAlphaEqual _ _ (TermCtor v1 _) (TermCtor v2 _) = varId v1 == varId v2
doPreTermsAlphaEqual _ _ TermUnitElem TermUnitElem = True
doPreTermsAlphaEqual _ _ TermUnitTy TermUnitTy = True
doPreTermsAlphaEqual _ _ TermEmpty TermEmpty = True
doPreTermsAlphaEqual _ _ TermTy TermTy = True
doPreTermsAlphaEqual bi r (TermArrow io1 as1 a1) (TermArrow io2 as2 a2) =
  io1 == io2
  && length as1 == length as2
  && let as = zip as1 as2
         su = foldl addArrowSubst IntMap.empty as
         as2' = map (substArrow su) as2
     in and (map (uncurry (doPreTermsAlphaEqual bi r)) (zip (map snd as1) (map snd as2')))
        && doPreTermsAlphaEqual bi r a1 (substPreTerm r su a2)
  where
    addArrowSubst ::
      SubstMap -> ((Maybe Var, PreTerm), (Maybe Var, PreTerm)) -> SubstMap
    addArrowSubst m ((Nothing, _), _) = m
    addArrowSubst m (_, (Nothing, _)) = m
    addArrowSubst m ((Just v1, _), (Just v2, _)) =
      IntMap.insert (varId v2) (TermVar False v1) m

    substArrow :: SubstMap -> (Maybe Var, PreTerm) -> (Maybe Var, PreTerm)
    substArrow su (Just v, t) =
      let v' = case IntMap.lookup (varId v) su of
                Nothing -> v
                Just (TermVar _ x) -> x
                _ -> error "unexpected term in subst map"
      in (Just v', substPreTerm r su t)
    substArrow su (Nothing, t) = (Nothing, substPreTerm r su t)
doPreTermsAlphaEqual bi r (TermLazyArrow io1 a1) (TermLazyArrow io2 a2) =
  io1 == io2 && doPreTermsAlphaEqual bi r a1 a2
doPreTermsAlphaEqual bi r (TermApp io1 f xs) (TermApp io2 g ys) =
  io1 == io2
  && length xs == length ys
  && doPreTermsAlphaEqual bi r f g
  && and (map (uncurry (doPreTermsAlphaEqual bi r)) (zip xs ys))
doPreTermsAlphaEqual bi r (TermImplicitApp _ f xs) (TermImplicitApp _ g ys) =
  let (xs0, xs1) = unzip (sortOn fst xs)
      (ys0, ys1) = unzip (sortOn fst ys)
  in xs0 == ys0
     && doPreTermsAlphaEqual bi r f g
     && and (map (uncurry (doPreTermsAlphaEqual bi r)) (zip xs1 ys1))
doPreTermsAlphaEqual bi r (TermLazyApp io1 f) (TermLazyApp io2 g) =
  io1 == io2 && doPreTermsAlphaEqual bi r f g
doPreTermsAlphaEqual bi r (TermLazyFun io1 t1) (TermLazyFun io2 t2) =
  io1 == io2 && doPreTermsAlphaEqual bi r t1 t2
doPreTermsAlphaEqual bi r (TermFun ias1 io1 _ ct1) (TermFun ias2 io2 _ ct2) =
  -- Number of explicit arguments are checked by case tree comparison.
  -- The order of the implicit arguments matters, since the case trees
  -- depend on the order.
  io1 == io2 && ias1 == ias2 && caseTreesAlphaEqual bi IntMap.empty r ct1 ct2
doPreTermsAlphaEqual bi r (TermRef v1 s1') (TermRef v2 s2')
  | isNothing (IntMap.lookup (varId v1) r)
    && isNothing (IntMap.lookup (varId v2) r) = varId v1 == varId v2
  | isNothing (IntMap.lookup (varId v1) r)
    && isJust (IntMap.lookup (varId v2) r) = False
  | isJust (IntMap.lookup (varId v1) r)
    && isNothing (IntMap.lookup (varId v2) r) = False
  | True =
  let s1 = IntMap.restrictKeys s1' (preTermVars r (TermRef v1 IntMap.empty))
      s2 = IntMap.restrictKeys s2' (preTermVars r (TermRef v2 IntMap.empty))
      p1 = (v1, s1)
      p2 = (v2, s2)
  in termRefsTest p1 p2
     || bisimHasPair p1 p2 bi
     || let r1 = Env.forceLookupRefMap (varId v1) r
            r2 = Env.forceLookupRefMap (varId v2) r
            r1' = substPreTerm r s1 (termPre r1)
            r2' = substPreTerm r s2 (termPre r2)
        in doPreTermsAlphaEqual ((p1, p2) : bi) r r1' r2'
  where
    termRefsTest :: (Var, SubstMap) -> (Var, SubstMap) -> Bool
    termRefsTest (u1, r1) (u2, r2) = do
      varId u1 == varId u2
      && IntMap.keysSet r1 == IntMap.keysSet r2
         && let g1 = IntMap.toList r1
                g2 = IntMap.toList r2
                z = zip g1 g2
            in and (map (\(x1, x2) ->
                     doPreTermsAlphaEqual bi r (snd x1) (snd x2)) z)

    bisimHasPair :: (Var, SubstMap) -> (Var, SubstMap) -> RefBisim -> Bool
    bisimHasPair _ _ [] = False
    bisimHasPair x1 x2 ((y1, y2) : bi') =
      (termRefsTest x1 y1 && termRefsTest x2 y2) || bisimHasPair x1 x2 bi'
doPreTermsAlphaEqual bi r (TermRef v1 s1) t2
  | isNothing (IntMap.lookup (varId v1) r) = False
  | True =
  let r1 = Env.forceLookupRefMap (varId v1) r
      r1' = substPreTerm r s1 (termPre r1)
  in doPreTermsAlphaEqual bi r r1' t2
doPreTermsAlphaEqual bi r t1 (TermRef v2 s2)
  | isNothing (IntMap.lookup (varId v2) r) = False
  | True =
  let r2 = Env.forceLookupRefMap (varId v2) r
      r2' = substPreTerm r s2 (termPre r2)
  in doPreTermsAlphaEqual bi r t1 r2'
doPreTermsAlphaEqual bi r (TermLazyFun io t1) t2 =
  doPreTermsAlphaEqual bi r t1 (TermLazyApp io t2)
doPreTermsAlphaEqual bi r t1 (TermLazyFun io t2) =
  doPreTermsAlphaEqual bi r (TermLazyApp io t1) t2
doPreTermsAlphaEqual bi r (TermFun [] io1 _ (CaseLeaf i1 t1 _)) t2 =
  doPreTermsAlphaEqual bi r
    t1 (TermApp io1 t2 (map (\i -> TermVar False (mkVar (varId i) "_")) i1))
doPreTermsAlphaEqual bi r t1 (TermFun [] io2 _ (CaseLeaf i2 t2 _)) =
  doPreTermsAlphaEqual bi r
    (TermApp io2 t1 (map (\i -> TermVar False (mkVar (varId i) "_")) i2)) t2
doPreTermsAlphaEqual bi r (TermCase t1 ct1) (TermCase t2 ct2) =
  doPreTermsAlphaEqual bi r t1 t2
  && caseTreesAlphaEqual bi IntMap.empty r ct1 ct2
doPreTermsAlphaEqual _ _ _ _ = False

caseTreesAlphaEqual ::
  RefBisim -> SubstMap -> RefMap -> CaseTree -> CaseTree -> Bool
caseTreesAlphaEqual bi subst rm (CaseLeaf i1 t1 _) (CaseLeaf i2 t2 _) = do
  length i1 == length i2
  && let su0 = if map varId i1 /= map varId i2
                then IntMap.fromList (zip (map varId i2) (map (TermVar False) i1))
                else IntMap.empty
         su = IntMap.union subst su0
     in doPreTermsAlphaEqual bi rm t1 (substPreTerm rm su t2)
caseTreesAlphaEqual bi subst rm (CaseNode idx1 m1 d1) (CaseNode idx2 m2 d2) =
  idx1 == idx2
  && IntMap.keysSet m1 == IntMap.keysSet m2
  && isJust d1 == isJust d2
  && let g1 = IntMap.toList m1
         g2 = IntMap.toList m2
         z = zip g1 g2
      in and (map (\(x1, x2) ->
               caseTreeInstanceAlphaEqual bi subst rm (snd x1) (snd x2)) z)
         && case (d1, d2) of
              (Nothing, Nothing) -> True
              (Just d1', Just d2') ->
                caseTreeInstanceAlphaEqual bi subst rm d1' d2'
              _ -> error "unexpected case"
caseTreesAlphaEqual bi subst rm (CaseUnit idx1 d1) (CaseUnit idx2 d2) =
  idx1 == idx2 && caseTreeInstanceAlphaEqual bi subst rm d1 d2
caseTreesAlphaEqual _ _ _ (CaseEmpty idx1) (CaseEmpty idx2) =
  idx1 == idx2
caseTreesAlphaEqual _ _ _ _ _ = False

caseTreeInstanceAlphaEqual ::
  RefBisim -> SubstMap -> RefMap -> ([Var], CaseTree) -> ([Var], CaseTree) -> Bool
caseTreeInstanceAlphaEqual bi subst rm (i1, c1) (i2, c2) =
  length i1 == length i2
  && let su0 = if map varId i1 /= map varId i2
                then IntMap.fromList (zip (map varId i2) (map (TermVar False) i1))
                else IntMap.empty
         su = IntMap.union subst su0
     in caseTreesAlphaEqual bi su rm c1 c2

preTermNormalize :: RefMap -> PreTerm -> PreTerm
preTermNormalize _ t@TermUnitElem = t
preTermNormalize _ t@TermUnitTy = t
preTermNormalize _ t@TermEmpty = t
preTermNormalize _ t@TermTy = t
preTermNormalize _ t@(TermCtor _ _) = t
preTermNormalize _ t@(TermData _) = t
preTermNormalize _ t@(TermVar _ _) = t
preTermNormalize _ t@(TermLazyFun True _) = t
preTermNormalize m (TermLazyFun False t) =
  TermLazyFun False (preTermNormalize m t)
preTermNormalize m (TermFun is False n (CaseLeaf vs t ws)) =
  TermFun is False n (CaseLeaf vs (preTermNormalize m t) ws)
preTermNormalize _ t@(TermFun _ _ _ _) = t
preTermNormalize m (TermArrow io d c) =
  TermArrow io (map normSnd d) (preTermNormalize m c)
  where normSnd (x, t) = (x, preTermNormalize m t)
preTermNormalize m (TermLazyArrow io c) =
  TermLazyArrow io (preTermNormalize m c)
preTermNormalize m (TermRef v s) =
  let t0 = IntMap.lookup (varId v) m
  in case t0 of
      Nothing -> TermRef v s
      Just (t, meta) ->
        let t' = substPreTerm m s (termPre t)
        in if refMetaIsDeclaredPure meta
           then preTermNormalize m t'
           else t'
preTermNormalize m (TermImplicitApp b f xs) =
  let f' = preTermNormalize m f
      xs' = map (\(n, x) -> (n, preTermNormalize m x)) xs
  in case f' of
      TermFun _ _ Nothing ct ->
        case caseTreeNormalize m ct (map snd xs') of
          Nothing -> TermImplicitApp b (normalizeAppBase f f') xs'
          Just r -> preTermNormalize m r
      TermFun _ io n (CaseLeaf is te ws) ->
        let z = zip (map varId is) (map snd xs')
            is' = drop (length z) is
            te' = substPreTerm m (IntMap.fromList z) te
        in TermFun [] io n (CaseLeaf is' te' ws)
      _ -> TermImplicitApp b f' xs'
preTermNormalize m (TermApp io f xs) =
  let f' = preTermNormalize m f
      xs' = map (preTermNormalize m) xs
  in case f' of
      TermFun [] _ _ ct ->
        case caseTreeNormalize m ct xs' of
          Nothing -> TermApp io (normalizeAppBase f f') xs'
          Just r -> preTermNormalize m r
      TermImplicitApp _ (TermFun _ _ _ ct) ys -> do
        case caseTreeNormalize m ct (map snd ys ++ xs') of
          Nothing -> TermApp io (normalizeAppBase f f') xs'
          Just r -> preTermNormalize m r
      _ -> TermApp io f' xs'
preTermNormalize m (TermLazyApp io f) =
  let f' = preTermNormalize m f
  in case f' of
      TermLazyFun _ t -> preTermNormalize m t
      _ -> TermLazyApp io f'
preTermNormalize m (TermCase t ct) =
  let t' = preTermNormalize m t
  in case caseTreeNormalize m ct [t'] of
      Nothing -> TermCase t' ct
      Just r -> preTermNormalize m r

normalizeAppBase :: PreTerm -> PreTerm -> PreTerm
normalizeAppBase unnormalized@(TermRef _ _) _ = unnormalized
normalizeAppBase _ normalized = normalized

caseTreeNormalize :: RefMap -> CaseTree -> [PreTerm] -> Maybe PreTerm
caseTreeNormalize rm = doCaseTreeNormalize rm IntMap.empty

doCaseTreeNormalize ::
  RefMap -> SubstMap -> CaseTree -> [PreTerm] -> Maybe PreTerm
doCaseTreeNormalize rm subst (CaseLeaf is te _) xs =
  let s = assert (length xs == length is) (zip (map varId is) xs)
      subst' = IntMap.union subst (IntMap.fromList s)
  in Just (substPreTerm rm subst' te)
doCaseTreeNormalize rm subst (CaseUnit idx (is, ct)) xs =
  let (x, xs') = listRemoveIdx idx xs
      a = zip (map varId is) (repeat x)
      subst' = IntMap.union subst (IntMap.fromList a)
  in doCaseTreeNormalize rm subst' ct xs'
doCaseTreeNormalize rm subst (CaseNode idx m d) xs = do
  let (x, xs') = listRemoveIdx idx xs
  (v, as) <- preTermProjArgs x
  let xs'' = as ++ xs'
  case IntMap.lookup (varId v) m of
    Nothing ->
      case d of
        Just (dis, dct) ->
          let a = zip (map varId dis) (repeat x)
              subst' = IntMap.union subst (IntMap.fromList a)
          in doCaseTreeNormalize rm subst' dct xs'
        Nothing ->
          error $ "missing ctor- and default-case for " ++ varName v
    Just (is, ct) ->
      let a = zip (map varId is) (repeat x)
          subst' = IntMap.union subst (IntMap.fromList a)
      in doCaseTreeNormalize rm subst' ct xs''
doCaseTreeNormalize _ _ (CaseEmpty _) _ = Nothing

listRemoveIdx :: Int -> [a] -> (a, [a])
listRemoveIdx 0 (x:xs) = (x, xs)
listRemoveIdx i (x:xs) =
  let (x', xs') = listRemoveIdx (i - 1) xs in (x', x : xs')
listRemoveIdx _ _ = error "incompatible listRemoveIdx index with list"

-- Normalize, then get Xn from X1 -> X2 -> ... -> Xn, where n = 1 is allowed.
-- Bool = True if IO arrow on the way.
preTermFinalCod :: RefMap -> PreTerm -> (PreTerm, Bool)
preTermFinalCod = \r t -> doPreTermFinalCod (preTermNormalize r t)
  where
    doPreTermFinalCod :: PreTerm -> (PreTerm, Bool)
    doPreTermFinalCod (TermArrow io _ c) =
      let (p, io') = doPreTermFinalCod c in (p, io || io')
    doPreTermFinalCod (TermLazyArrow io c) =
      let (p, io') = doPreTermFinalCod c in (p, io || io')
    doPreTermFinalCod t = (t, False)

preTermDomCod ::
  RefMap -> PreTerm -> Maybe ([(Maybe Var, PreTerm)], PreTerm, Bool)
preTermDomCod = \r t ->
  case t of
    TermArrow io d c -> Just (d, c, io)
    _ -> doPreTermDomCod (preTermNormalize r t)
  where
    doPreTermDomCod ::
      PreTerm -> Maybe ([(Maybe Var, PreTerm)], PreTerm, Bool)
    doPreTermDomCod (TermArrow io d c) = Just (d, c, io)
    doPreTermDomCod _ = Nothing

preTermLazyCod :: RefMap -> PreTerm -> Maybe (PreTerm, Bool)
preTermLazyCod = \r t ->
  case t of
    TermLazyArrow io c -> Just (c, io)
    _ -> doPreTermLazyCod (preTermNormalize r t)
  where
    doPreTermLazyCod :: PreTerm -> Maybe (PreTerm, Bool)
    doPreTermLazyCod (TermLazyArrow io c) = Just (c, io)
    doPreTermLazyCod _ = Nothing

-- Returns (True, t, io) when lazy
-- Returns (False, t, io) when delay arrow
preTermLazyOrDelayCod :: RefMap -> PreTerm -> Maybe (Bool, PreTerm, Bool)
preTermLazyOrDelayCod = \r t ->
  case t of
    TermLazyArrow io c -> Just (True, c, io)
    TermArrow io [] c -> Just (False, c, io)
    _ -> doPreTermLazyOrDelayCod (preTermNormalize r t)
  where
    doPreTermLazyOrDelayCod :: PreTerm -> Maybe (Bool, PreTerm, Bool)
    doPreTermLazyOrDelayCod (TermLazyArrow io c) = Just (True, c, io)
    doPreTermLazyOrDelayCod (TermArrow io [] c) = Just (False, c, io)
    doPreTermLazyOrDelayCod _ = Nothing

preTermCodRootType :: RefMap -> PreTerm -> Maybe (PreTerm, Bool)
preTermCodRootType rm pt =
  let (p, b) = preTermFinalCod rm pt
  in fmap (\q -> (q, b)) (doGetRootType p)
  where
    doGetRootType :: PreTerm -> Maybe PreTerm
    doGetRootType (TermApp _ t _) = doGetRootType t
    doGetRootType (TermImplicitApp _ t _) = doGetRootType t
    doGetRootType (TermLazyApp _ t) = doGetRootType t
    doGetRootType (TermArrow _ _ _) = error "unreachable case"
    doGetRootType (TermLazyArrow _ _) = error "unreachable case"
    doGetRootType (TermFun _ _ _ _) = Nothing
    doGetRootType (TermLazyFun _ _) = Nothing
      -- If it is a ref after normalization, then we are unable to
      -- determine its root type. So do not unfold the ref:
    doGetRootType (TermRef _ _) = Nothing
    doGetRootType (TermVar b v) = Just (TermVar b v)
    doGetRootType (TermData v) = Just (TermData v)
    doGetRootType (TermCtor _ _) = Nothing
    doGetRootType (TermCase _ _) = Nothing
    doGetRootType TermUnitElem = Nothing
    doGetRootType TermUnitTy = Just TermUnitTy
    doGetRootType TermEmpty = Nothing
    doGetRootType TermTy = Just TermTy

preTermPartialApplication :: 
  Bool -> Int -> PreTerm -> [PreTerm] -> Bool -> TypeCheckIO PreTerm
preTermPartialApplication appIo numMissing f args fio = do
  let !() = assert (numMissing > 0) ()
  avs <- newAppliedVars args
  vs <- newUnappliedVars
  let apArgs = map (TermVar False . fst) avs ++ map (TermVar False) vs
  let e = TermApp appIo f apArgs
  let fpart = TermFun [] (appIo || fio)
                (Just (length vs)) (CaseLeaf vs e [])
  appliedCaseExpr avs fpart
  where
    appliedCaseExpr :: [(Var, PreTerm)] -> PreTerm -> TypeCheckIO PreTerm
    appliedCaseExpr [] fpart = return fpart
    appliedCaseExpr ((v, a) : as) fpart = do
      t <- appliedCaseExpr as fpart
      return (TermCase a (CaseLeaf [v] t []))

    newAppliedVars :: [PreTerm] -> TypeCheckIO [(Var, PreTerm)]
    newAppliedVars [] = return []
    newAppliedVars (a : as) = do
      i <- Env.freshVarId
      l <- Env.getNextLocalVarIdStr
      let v = mkVar i ("_x" ++ l)
      vs <- newAppliedVars as
      return ((v, a) : vs)

    newUnappliedVars :: TypeCheckIO [Var]
    newUnappliedVars = makeNewVars 0

    makeNewVars :: Int -> TypeCheckIO [Var]
    makeNewVars n =
      if n == numMissing
      then return []
      else do
        i <- Env.freshVarId
        l <- Env.getNextLocalVarIdStr
        vs <- makeNewVars (n + 1)
        let v = mkVar i ("_x" ++ l)
        return (v : vs)

prePatternApplyWithVars ::
  PrePattern -> PreTerm -> TypeCheckIO Pattern
prePatternApplyWithVars ctor = \ty -> do
  ias <- case ctor of
          PatternCtor v _ -> do
            imps <- Env.forceLookupImplicit (varId v)
            mapM addArg imps
          _ -> return []
  as <- getVars ty
  im <- Env.getImplicitMap
  rm <- Env.getRefMap
  let p = Pattern {patternPre = ctor, patternTy = ty}
  return (patternApply im rm p ias as)
  where
    addArg :: (Var, PreTerm) -> TypeCheckIO (VarName, PrePattern)
    addArg (n, _) = do
      i <- Env.freshVarId
      l <- Env.getNextLocalVarIdStr
      let v = mkVar i ("_x" ++ l)
      let a = PatternVar v
      return (varName n, a)

    addArg1 :: (Maybe Var, PreTerm) -> TypeCheckIO PrePattern
    addArg1 _ = do
      i <- Env.freshVarId
      l <- Env.getNextLocalVarIdStr
      let v = mkVar i ("_x" ++ l)
      let a = PatternVar v
      return a

    getVars :: PreTerm -> TypeCheckIO [Maybe [PrePattern]]
    getVars ty = do
      rm <- Env.getRefMap
      case preTermDomCod rm ty of
        Nothing ->
          case preTermLazyCod rm ty of
            Nothing -> return []
            Just (c0, _) -> do
              as <- getVars c0
              return (Nothing : as)
        Just (d, c0, _) -> do
          as <- mapM addArg1 d
          ass <- getVars c0
          return (Just as : ass)

patternApply ::
  ImplicitMap -> RefMap -> Pattern -> [(VarName, PrePattern)] ->
  [Maybe [PrePattern]] -> Pattern
patternApply im rm p0 = \is as ->
  doPatternApply (patternTy p0) (patternPre p0) is as
  where
    doPatternApply ::
      PreTerm -> PrePattern -> [(VarName, PrePattern)] ->
      [Maybe [PrePattern]] -> Pattern
    doPatternApply ty p [] [] = Pattern {patternPre = p, patternTy = ty}
    doPatternApply ty p is@(_:_) as =
      case p of
        PatternCtor v _ ->
          let imps = Env.forceLookupImplicitMap (varId v) im
              z = zip (map snd is) (map (\(x, t) -> (Just x, t)) imps)
              su = makePatternArgSubst (assert (length imps == length is) z)
              c = substPreTerm rm su ty
          in doPatternApply c (PatternImplicitApp True p is) [] as
        _ -> error "implicit arguments on non-ctor pattern"
    doPatternApply ty p [] (Nothing : as) =
      case preTermLazyCod rm ty of
        Nothing ->
          error "attempt to lazy apply non-lazy pattern"
        Just (c, _) -> doPatternApply c (PatternLazyApp p) [] as
    doPatternApply ty p [] (Just a : as) =
      case preTermDomCod rm ty of
        Nothing ->
          error "attempt to apply non-function pattern"
        Just (d, c', _) ->
          let su = makePatternArgSubst
                    (assert (length a == length d) $ zip a d)
              c = substPreTerm rm su c'
          in doPatternApply c (PatternApp p a) [] as

patternProjArgs ::
  ImplicitMap -> RefMap -> Pattern -> ([(VarName, Pattern)], [Maybe [Pattern]])
patternProjArgs im rm pat =
  let (is, ps) = snd (doProj (patternPre pat)) in (is, reverse ps)
  where
    doProj :: PrePattern -> (PreTerm, ([(VarName, Pattern)], [Maybe [Pattern]]))
    doProj (PatternCtor v _) =
      let x = Env.forceLookupRefMap (varId v) rm
      in (termTy x, ([], []))
    doProj (PatternImplicitApp _ p as) =
      let (ty, (_, ps)) = doProj p
          imps = case p of
                  PatternCtor v _ -> Env.forceLookupImplicitMap (varId v) im
                  _ -> error "implicit args on non-ctor pattern"
          xs = zip (map snd as) (map (\(x,y) -> (Just x, y)) imps)
          su = makePatternArgSubst xs
          as' = map (\(a, (_, d)) ->
                        Pattern {patternPre = a,
                                 patternTy = substPreTerm rm su d}) xs
      in (substPreTerm rm su ty, (zip (map fst as) as', ps))
    doProj (PatternLazyApp p) =
      let (ty, (b, ps)) = doProj p
          (c, _) = fromJust (preTermLazyCod rm ty)
      in (c, (b, Nothing : ps))
    doProj (PatternApp p as) =
      let (ty, (b, ps)) = doProj p
          (ds, c, _) = fromJust (preTermDomCod rm ty)
          xs = zip as ds
          su = makePatternArgSubst xs
          as' = map (\(a, (_, d)) ->
                        Pattern {patternPre = a,
                                 patternTy = substPreTerm rm su d}) xs
      in (substPreTerm rm su c, (b, Just as' : ps))
    doProj _ = (patternTy pat, ([], []))

preTermProjArgs :: PreTerm -> Maybe (Var, [PreTerm])
preTermProjArgs (TermCtor v _) = Just (v, [])
preTermProjArgs (TermLazyApp _ f) = preTermProjArgs f
preTermProjArgs (TermImplicitApp _ f xs) = do
  (v, as) <- preTermProjArgs f
  Just (v, as ++ map snd xs)
preTermProjArgs (TermApp _ f xs) = do
  (v, as) <- preTermProjArgs f
  Just (v, as ++ xs)
preTermProjArgs _ = Nothing

makePatternArgSubst :: [(PrePattern, (Maybe Var, PreTerm))] -> SubstMap
makePatternArgSubst [] = IntMap.empty
makePatternArgSubst ((_, (Nothing, _)) : xs) = makePatternArgSubst xs
makePatternArgSubst ((a, (Just v, _)) : xs) =
  IntMap.insert (varId v) (prePatternToPreTerm a) (makePatternArgSubst xs)

preTermExistsVar :: RefMap -> (VarId -> Bool) -> PreTerm -> Bool
preTermExistsVar rm p t =
  not (IntSet.null (IntSet.filter p (preTermVars rm t)))

preTermGetAlphaType :: Env.Env -> ImplicitMap -> RefMap -> Env.ImplicitVarMap -> PreTerm -> Maybe PreTerm
preTermGetAlphaType env im rm iv (TermLazyApp _ f) = do
  ty <- preTermGetAlphaType env im rm iv f
  (cod, _) <- preTermLazyCod rm ty
  return cod
preTermGetAlphaType env im rm iv (TermImplicitApp _ f xs) = do
  ty <- preTermGetAlphaType env im rm iv f
  is <- case f of
          TermData v -> return (Env.forceLookupImplicitMap (varId v) im)
          TermCtor v _ -> return (Env.forceLookupImplicitMap (varId v) im)
          TermRef v _ -> return (Env.forceLookupImplicitMap (varId v) im)
          _ -> Nothing
  let !() = assert (length is == length xs) ()
  let su = foldl addSubst IntMap.empty (zip is xs)
  return (substPreTerm rm su ty)
  where
    addSubst :: SubstMap -> ((Var, PreTerm), (VarName, PreTerm)) -> SubstMap
    addSubst s ((v, _), (_, t)) = IntMap.insert (varId v) t s
preTermGetAlphaType env im rm iv (TermApp _ f xs) = do
  ty <- preTermGetAlphaType env im rm iv f
  (dom, cod, _) <- preTermDomCod rm ty
  let !() = assert (length dom == length xs) ()
  let su = foldl addSubst IntMap.empty (zip dom xs)
  return (substPreTerm rm su cod)
  where
    addSubst :: SubstMap -> ((Maybe Var, PreTerm), PreTerm) -> SubstMap
    addSubst s ((Nothing, _), _) = s
    addSubst s ((Just v, _), t) = IntMap.insert (varId v) t s
preTermGetAlphaType _ _ rm _ (TermData v) = do
  fmap (termTy . fst) (IntMap.lookup (varId v) rm)
preTermGetAlphaType _ _ rm _ (TermCtor v _) =
  fmap (termTy . fst) (IntMap.lookup (varId v) rm)
preTermGetAlphaType _ _ rm _ (TermRef v _) = 
  fmap (termTy . fst) (IntMap.lookup (varId v) rm)
preTermGetAlphaType env _ _ iv (TermVar _ v) = do
    fmap fst (IntMap.lookup (varId v) iv) <|> Env.findTermVarType v env
preTermGetAlphaType env im rm iv (TermLazyFun io f) =
  fmap (TermLazyArrow io) (preTermGetAlphaType env im rm iv f)
preTermGetAlphaType env im rm iv (TermFun [] io (Just _) (CaseLeaf vs f _)) = do
  ds <- mapM (inferVarAlphaType env im rm iv f) vs
  fmap (TermArrow io (zip (map Just vs) ds)) (preTermGetAlphaType env im rm iv f)
preTermGetAlphaType _ _ _ _ (TermArrow _ _ _) = Just TermTy
preTermGetAlphaType _ _ _ _ (TermLazyArrow _ _) = Just TermTy
preTermGetAlphaType _ _ _ _ TermUnitElem = Just TermUnitTy
preTermGetAlphaType _ _ _ _ TermUnitTy = Just TermTy
preTermGetAlphaType _ _ _ _ TermTy = Just TermTy
preTermGetAlphaType _ _ _ _ (TermFun _ _ _ _) = Nothing
preTermGetAlphaType _ _ _ _ (TermCase _ _) = Nothing
preTermGetAlphaType _ _ _ _ TermEmpty = Nothing

preTermIsThisVar :: Var -> PreTerm -> Bool
preTermIsThisVar v (TermVar False w) = varId v == varId w
preTermIsThisVar _ _ = False

inferVarAlphaType :: Env.Env -> ImplicitMap -> RefMap -> Env.ImplicitVarMap -> PreTerm -> Var -> Maybe PreTerm
inferVarAlphaType env im rm iv (TermLazyApp _ f) v = inferVarAlphaType env im rm iv f v
inferVarAlphaType env im rm iv (TermImplicitApp _ f xs) v = do
  inferVarAlphaType env im rm iv f v
  <|> (findIndex (preTermIsThisVar v . snd) xs >>= getFromArgs)
  where
    getFromArgs :: Int -> Maybe PreTerm
    getFromArgs i = do
      is <- case f of
              TermData x -> return (Env.forceLookupImplicitMap (varId x) im)
              TermCtor x _ -> return (Env.forceLookupImplicitMap (varId x) im)
              TermRef x _ -> return (Env.forceLookupImplicitMap (varId x) im)
              _ -> Nothing
      let !() = assert (length is == length xs) ()
      Just (snd (is !! i))
inferVarAlphaType env im rm iv (TermApp _ f xs) v = do
  inferVarAlphaType env im rm iv f v
  <|> (findIndex (preTermIsThisVar v) xs >>= getFromArgs)
  where
    getFromArgs :: Int -> Maybe PreTerm
    getFromArgs i = do
      t <- preTermGetAlphaType env im rm iv f
      case t of
        TermArrow _ d _ -> do
          let !() = assert (length d == length xs) ()
          Just (snd (d !! i))
        _ -> error "expected type of applied term to be arrow"
inferVarAlphaType env im rm iv (TermLazyFun _ f) v =
  inferVarAlphaType env im rm iv f v
inferVarAlphaType env im rm iv (TermFun _ _ _ (CaseLeaf _ f _)) v = do
  inferVarAlphaType env im rm iv f v
inferVarAlphaType _ _ _ _ (TermData _) _ = Nothing
inferVarAlphaType _ _ _ _ (TermCtor _ _) _ = Nothing
inferVarAlphaType _ _ _ _ (TermRef _ _) _ = Nothing
inferVarAlphaType _ _ _ _ (TermVar _ _) _ = Nothing
inferVarAlphaType _ _ _ _ (TermArrow _ _ _) _ = Nothing
inferVarAlphaType _ _ _ _ (TermLazyArrow _ _) _ = Nothing
inferVarAlphaType _ _ _ _ TermUnitElem _ = Nothing
inferVarAlphaType _ _ _ _ TermUnitTy _ = Nothing
inferVarAlphaType _ _ _ _ TermTy _ = Nothing
inferVarAlphaType _ _ _ _ (TermFun _ _ _ _) _ = Nothing
inferVarAlphaType _ _ _ _ (TermCase _ _) _ = Nothing
inferVarAlphaType _ _ _ _ TermEmpty _ = Nothing

getAppAlphaArgumentWeight ::
  RefMap -> Env.ImplicitVarMap -> PreTerm -> (Int, Int, Int)
getAppAlphaArgumentWeight rm iv t =
  let p = getAppliedImplicits IntSet.empty rm t
      a = IntSet.difference allImplicits p
  in (IntSet.size p, -rigidImplicits, IntSet.size a - rigidImplicits)
  where
    allImplicits :: IntSet
    allImplicits = IntSet.filter (flip IntMap.member iv) (preTermVars rm t)

    rigidImplicits :: Int
    rigidImplicits =
      case doRigidImplicits t of
        Nothing -> 0
        Just s -> IntSet.size s

    doRigidImplicits :: PreTerm -> Maybe IntSet
    doRigidImplicits (TermVar True v) = Just (IntSet.singleton (varId v))
    doRigidImplicits (TermLazyApp _ f) = doRigidImplicits f
    doRigidImplicits (TermApp _ f xs) = do
      f' <- doRigidImplicits f
      Just $ foldl (\a x ->
                      case doRigidImplicits x of
                        Nothing -> IntSet.empty
                        Just s -> IntSet.union a s
                   ) f' xs
    doRigidImplicits (TermImplicitApp _ f xs) = do
      doRigidImplicits (TermApp False f (map snd xs))
    doRigidImplicits (TermCtor _ _) = Just IntSet.empty
    doRigidImplicits (TermData _) = Just IntSet.empty
    doRigidImplicits (TermLazyArrow _ c) = doRigidImplicits c
    doRigidImplicits (TermArrow _ _ c) = doRigidImplicits c
    doRigidImplicits TermUnitElem = Just IntSet.empty
    doRigidImplicits TermUnitTy = Just IntSet.empty
    doRigidImplicits TermTy = Just IntSet.empty
    doRigidImplicits _ = Nothing

getAppliedImplicits :: IntSet -> RefMap -> PreTerm -> IntSet
getAppliedImplicits vis rm (TermLazyApp _ f) =
  getAppliedImplicits vis rm f
getAppliedImplicits vis rm (TermImplicitApp _ f xs) =
  IntSet.union
    (getAppliedImplicits vis rm f)
    (foldl (\a x ->
      IntSet.union a $ getAppliedImplicits vis rm (snd x)) IntSet.empty xs)
getAppliedImplicits vis rm (TermApp _ (TermVar True v) xs) =
  IntSet.insert
    (varId v)
    (foldl (\a x ->
      IntSet.union a $ getAppliedImplicits vis rm x) IntSet.empty xs)
getAppliedImplicits vis rm (TermApp _ f xs) = do
  IntSet.union
    (getAppliedImplicits vis rm f)
    (foldl (\a x ->
      IntSet.union a $ getAppliedImplicits vis rm x) IntSet.empty xs)
getAppliedImplicits vis rm (TermLazyFun _ f) =
  getAppliedImplicits vis rm f
getAppliedImplicits _ _ (TermData _) = IntSet.empty
getAppliedImplicits _ _ (TermCtor _ _) = IntSet.empty
getAppliedImplicits vis rm (TermRef v s) =
  if IntSet.member (varId v) vis
  then IntSet.empty
  else
    let t0 = IntMap.lookup (varId v) rm
    in case t0 of
        Nothing -> IntSet.empty
        Just (t, _) ->
          let t' = substPreTerm rm s (termPre t)
          in getAppliedImplicits (IntSet.insert (varId v) vis) rm t'
getAppliedImplicits vis rm (TermArrow _ d c) =
  IntSet.union
    (getAppliedImplicits vis rm c)
    (foldl (\a x ->
      IntSet.union a $ getAppliedImplicits vis rm (snd x)) IntSet.empty d)
getAppliedImplicits vis rm (TermLazyArrow _ c) =
  getAppliedImplicits vis rm c
getAppliedImplicits vis rm (TermFun _ _ _ ct) =
  getAppliedImplicitsCaseTree vis rm ct
getAppliedImplicits vis rm (TermCase t ct) =
  IntSet.union
    (getAppliedImplicits vis rm t)
    (getAppliedImplicitsCaseTree vis rm ct)
getAppliedImplicits _ _ (TermVar _ _) = IntSet.empty
getAppliedImplicits _ _ TermUnitElem = IntSet.empty
getAppliedImplicits _ _ TermUnitTy = IntSet.empty
getAppliedImplicits _ _ TermTy = IntSet.empty
getAppliedImplicits _ _ TermEmpty = IntSet.empty

getAppliedImplicitsCaseTree ::
  IntSet -> RefMap -> CaseTree -> IntSet
getAppliedImplicitsCaseTree vis rm (CaseLeaf _ te _) =
  getAppliedImplicits vis rm te
getAppliedImplicitsCaseTree vis rm (CaseUnit _ (_, ct)) =
  getAppliedImplicitsCaseTree vis rm ct
getAppliedImplicitsCaseTree vis rm (CaseNode _ m d) =
  let d' = case d of
            Nothing -> IntSet.empty
            Just (_, ct) -> getAppliedImplicitsCaseTree vis rm ct
  in IntMap.foldl (\a (_, x) ->
      IntSet.union a $ getAppliedImplicitsCaseTree vis rm x) d' m
getAppliedImplicitsCaseTree _ _ (CaseEmpty _) = IntSet.empty

----------------- Pretty printing -------------------------------

preTermToString :: Int -> PreTerm -> TypeCheckIO String
preTermToString indent t =
  execWriterT (runStateT (writeIndent >> writePreTerm t) indent)

prePatternToString :: Int -> PrePattern -> TypeCheckIO String
prePatternToString indent t =
  preTermToString indent (prePatternToPreTerm t)

caseTreeToString :: Int -> CaseTree -> [PreTerm] -> TypeCheckIO String
caseTreeToString indent t as =
  execWriterT (runStateT (writeIndent >> writeCaseTree (map Right as) t) indent)

type ToString a = StateT Int (WriterT String TypeCheckIO) a

liftTT :: TypeCheckIO a -> ToString a
liftTT = lift . lift

incIndent :: ToString ()
incIndent = modify (\n -> n+2)

decIndent :: ToString ()
decIndent = modify (\n -> n-2)

nextVarName :: ToString VarName
nextVarName = liftTT (fmap ("_x"++) Env.getNextLocalVarIdStr)

writeIndent :: ToString ()
writeIndent = get >>= \n -> doWrite n
  where
    doWrite :: Int -> ToString ()
    doWrite 0 = return ()
    doWrite i = tell " " >> doWrite (i - 1)

isVerbose :: ToString Bool
isVerbose = liftTT isVerboseOn

writeStr :: String -> ToString ()
writeStr s = tell s

newLine :: ToString ()
newLine = tell "\n" >> writeIndent

writeFunImplicitNames :: [VarName] -> ToString ()
writeFunImplicitNames [] = return ()
writeFunImplicitNames [i] = do
  writeStr "["
  writeStr i
  writeStr "]"
writeFunImplicitNames (i : is) = do
  writeStr "["
  writeStr i
  writeStr "] "
  writeFunImplicitNames is

writePreTerm :: PreTerm -> ToString ()
writePreTerm (TermFun imps _ (Just _) (CaseLeaf vs t _)) = do
  writeFunImplicitNames imps
  when (not (null imps)) (writeStr " ")
  writeVarList (drop (length imps) vs)
  writeStr " => "
  writePreTerm t
writePreTerm (TermFun imps _ Nothing ct) = do
  writeFunImplicitNames imps
  when (not (null imps)) (writeStr " => ")
  writeCaseTree (map Left imps) ct
writePreTerm (TermFun imps _ (Just n) ct) = do
  args <- mapM (\_ -> nextVarName) [1..n]
  writeFunImplicitNames imps
  when (not (null imps)) (writeStr " ")
  writeNameList args
  writeStr " => "
  writeCaseTree (map Left imps ++ map Left args) ct
writePreTerm (TermLazyFun _ f) =
  writeStr "[] => " >> writePreTerm f
writePreTerm (TermArrow io [] c) = do
  if io
    then writeStr "() ->> "
    else writeStr "() -> "
  writePreTerm c
writePreTerm (TermArrow io d c) = do
  writeDomainList d
  if io
    then writeStr " ->> "
    else writeStr " -> "
  writePreTerm c
writePreTerm (TermLazyArrow io t) = do
  if io
    then writeStr "[] ->> "
    else writeStr "[] -> "
  writePreTerm t
writePreTerm (TermApp _ f@(TermVar _ v) [x])
  | Str.isFixOp (varName v) =
      writeUnaryApp v f x
writePreTerm
    (TermApp a (TermImplicitApp False f@(TermVar _ v) b) [x])
  | Str.isFixOp (varName v) = do
      vb <- isVerbose
      if vb
      then writePreTerm (TermApp a (TermImplicitApp True f b) [x])
      else writeUnaryApp v f x
writePreTerm (TermApp _ f@(TermCtor v _) [x])
  | Str.isFixOp (varName v) =
      writeUnaryApp v f x
writePreTerm
    (TermApp a (TermImplicitApp False f@(TermCtor v _) b) [x])
  | Str.isFixOp (varName v) = do
      vb <- isVerbose
      if vb
      then writePreTerm (TermApp a (TermImplicitApp True f b) [x])
      else writeUnaryApp v f x
writePreTerm (TermApp _ f@(TermData v) [x])
  | Str.isFixOp (varName v) =
      writeUnaryApp v f x
writePreTerm
    (TermApp a (TermImplicitApp False f@(TermData v) b) [x])
  | Str.isFixOp (varName v) = do
      vb <- isVerbose
      if vb
      then writePreTerm (TermApp a (TermImplicitApp True f b) [x])
      else writeUnaryApp v f x
writePreTerm (TermApp _ f@(TermRef v _) [x])
  | Str.isFixOp (varName v) =
      writeUnaryApp v f x
writePreTerm
    (TermApp a (TermImplicitApp False f@(TermRef v _) b) [x])
  | Str.isFixOp (varName v) = do
      vb <- isVerbose
      if vb
      then writePreTerm (TermApp a (TermImplicitApp True f b) [x])
      else writeUnaryApp v f x
writePreTerm (TermApp _ f@(TermVar _ v) [x1, x2])
  | Str.isFixOp (varName v) =
      writeBinaryApp v f x1 x2
writePreTerm
    (TermApp a (TermImplicitApp False f@(TermVar _ v) b) [x1, x2])
  | Str.isFixOp (varName v) = do
      vb <- isVerbose
      if vb
      then writePreTerm (TermApp a (TermImplicitApp True f b) [x1, x2])
      else writeBinaryApp v f x1 x2
writePreTerm (TermApp _ f@(TermCtor v _) [x1, x2])
  | Str.isFixOp (varName v) =
      writeBinaryApp v f x1 x2
writePreTerm
    (TermApp a (TermImplicitApp False f@(TermCtor v _) b) [x1, x2])
  | Str.isFixOp (varName v) = do
      vb <- isVerbose
      if vb
      then writePreTerm (TermApp a (TermImplicitApp True f b) [x1, x2])
      else writeBinaryApp v f x1 x2
writePreTerm (TermApp _ f@(TermData v) [x1, x2])
  | Str.isFixOp (varName v) =
      writeBinaryApp v f x1 x2
writePreTerm
    (TermApp a (TermImplicitApp False f@(TermData v) b) [x1, x2])
  | Str.isFixOp (varName v) = do
      vb <- isVerbose
      if vb
      then writePreTerm (TermApp a (TermImplicitApp True f b) [x1, x2])
      else writeBinaryApp v f x1 x2
writePreTerm (TermApp _ f@(TermRef v _) [x1, x2])
  | Str.isFixOp (varName v) =
      writeBinaryApp v f x1 x2
writePreTerm
    (TermApp a (TermImplicitApp False f@(TermRef v _) b) [x1, x2])
  | Str.isFixOp (varName v) = do
      vb <- isVerbose
      if vb
      then writePreTerm (TermApp a (TermImplicitApp True f b) [x1, x2])
      else writeBinaryApp v f x1 x2
writePreTerm (TermApp _ f@(TermVar _ v) (x:xs))
  | Str.isPostfixOp (varName v) =
      writePostfixApp v f x xs
writePreTerm
    (TermApp a (TermImplicitApp False f@(TermVar _ v) b) (x:xs))
  | Str.isPostfixOp (varName v) = do
      vb <- isVerbose
      if vb
      then writePreTerm (TermApp a (TermImplicitApp True f b) (x:xs))
      else writePostfixApp v f x xs
writePreTerm (TermApp _ f@(TermCtor v _) (x:xs))
  | Str.isPostfixOp (varName v) =
      writePostfixApp v f x xs
writePreTerm
    (TermApp a (TermImplicitApp False f@(TermCtor v _) b) (x:xs))
  | Str.isPostfixOp (varName v) = do
      vb <- isVerbose
      if vb
      then writePreTerm (TermApp a (TermImplicitApp True f b) (x:xs))
      else writePostfixApp v f x xs
writePreTerm (TermApp _ f@(TermData v) (x:xs))
  | Str.isPostfixOp (varName v) =
      writePostfixApp v f x xs
writePreTerm
    (TermApp a (TermImplicitApp False f@(TermData v) b) (x:xs))
  | Str.isPostfixOp (varName v) = do
      vb <- isVerbose
      if vb
      then writePreTerm (TermApp a (TermImplicitApp True f b) (x:xs))
      else writePostfixApp v f x xs
writePreTerm (TermApp _ f@(TermRef v _) (x:xs))
  | Str.isPostfixOp (varName v) =
      writePostfixApp v f x xs
writePreTerm
    (TermApp a (TermImplicitApp False f@(TermRef v _) b) (x:xs))
  | Str.isPostfixOp (varName v) = do
      vb <- isVerbose
      if vb
      then writePreTerm (TermApp a (TermImplicitApp True f b) (x:xs))
      else writePostfixApp v f x xs
writePreTerm (TermApp _ f xs) = writeNormalApp f xs
writePreTerm (TermImplicitApp True f xs) = do
  let b = hasLowerPrecThanApp f
  when b (writeStr "(")
  writePreTerm f
  when b (writeStr ")")
  when (not (null xs)) (writeStr " ")
  writeNamedPreTermArgs xs
writePreTerm (TermImplicitApp False f xs) = do
  vb <- isVerbose
  if vb
  then writePreTerm (TermImplicitApp True f xs)
  else writePreTerm f
writePreTerm (TermLazyApp _ f) = do
  let b = hasLowerPrecThanApp f
  when b (writeStr "(")
  writePreTerm f
  when b (writeStr ")")
  writeStr " []"
writePreTerm (TermRef v _) = writeVarTerm v
writePreTerm (TermData v) = writeVarTerm v
writePreTerm (TermCtor v _) = writeVarTerm v
writePreTerm (TermVar _ v) = writeVarTerm v
writePreTerm TermUnitElem = writeStr "()"
writePreTerm TermUnitTy = writeStr "{}"
writePreTerm TermTy = writeStr "Ty"
writePreTerm TermEmpty = writeStr "{}"
writePreTerm (TermCase e ct) = writeCaseTree [Right e] ct

takeOperatorStr :: String -> String
takeOperatorStr = fst . Str.operandSplit

writeVarTerm :: Var -> ToString ()
writeVarTerm v = do
  let pre = Str.isFixOp (varName v)
  when pre (writeStr "(")
  vb <- isVerbose
  s <- if vb
       then return (varName v)
       else simplifyModule v
  writeStr s
  when vb $ do
    writeStr "_"
    writeStr (show (varId v))
  when pre (writeStr ")")

simplifyModule :: Var -> ToString VarName
simplifyModule v = do
  let na = varName v
  let (na0, mo0) = Str.moduleSplit na
  short <- canLookup na0
  if short
  then return (takeOperatorStr na0)
  else
    case mo0 of
      Nothing -> return (takeOperatorStr na)
      Just mo1 -> do
        exm <- liftTT Env.getModuleExpandMap
        findFromAlias na0 mo1 (sortBy shortLen (Map.keys exm))
  where
    findFromAlias ::
      VarName -> VarName -> [VarName] -> ToString VarName
    findFromAlias _ _ [] = return (takeOperatorStr (varName v))
    findFromAlias na mo (a : as) = do
      let (na0, op) = Str.operandSplit na
      let na1 = Str.operandConcatMaybe (na0 ++ "." ++ a) op
      hasAlias <- canLookup na1
      if hasAlias
      then return (na0 ++ "." ++ a)
      else findFromAlias na mo as

    canLookup :: VarName -> ToString Bool
    canLookup na = do
      modName <- liftTT currentModuleName
      st <-liftTT (Env.lookup modName na)
      case st of
        Just (Env.StatusTerm (Term { termPre = TermRef w _ })) ->
          return $ varId w == varId v
        Just (Env.StatusTerm (Term { termPre = TermData w })) ->
          return $ varId w == varId v
        Just (Env.StatusTerm (Term { termPre = TermCtor w _ })) ->
          return $ varId w == varId v
        _ ->
          return False

    shortLen :: VarName -> VarName -> Ordering
    shortLen n1 n2 = compare (length n1) (length n2)

writePostfixApp ::
  Var -> PreTerm -> PreTerm -> [PreTerm] -> ToString ()
writePostfixApp v _f x xs = do
  let b = hasLowerPrecThanApp x
  when b (writeStr "(")
  writePreTerm x
  when b (writeStr ")")
  writeStr " "
  na <- simplifyModule v
  writeStr na
  when (not (null xs)) $ do
    writeStr " "
    writePreTermList xs

writeUnaryApp ::
  Var -> PreTerm -> PreTerm -> ToString ()
writeUnaryApp v _f x = do
  na <- simplifyModule v
  writeStr na >> writeStr " "
  let b = hasLowerPrecThanPrefixOp x
  when b (writeStr "(")
  writePreTerm x
  when b (writeStr ")")

writeBinaryApp ::
  Var -> PreTerm -> PreTerm -> PreTerm -> ToString ()
writeBinaryApp v _f x1 x2
  | Str.isFixOp6 (varName v) = do
    let b1 = hasLowerPrecThanInfixOp6 x1
    when b1 (writeStr "(") >> writePreTerm x1 >> when b1 (writeStr ")")
    writeStr " "
    na <- simplifyModule v
    writeStr na
    writeStr " "
    let b2 = hasLowerPrecThanInfixOp5 x2
    when b2 (writeStr "(") >> writePreTerm x2 >> when b2 (writeStr ")")
  | Str.isFixOp5 (varName v) = do
    let b1 = hasLowerPrecThanInfixOp4 x1
    when b1 (writeStr "(") >> writePreTerm x1 >> when b1 (writeStr ")")
    writeStr " "
    na <- simplifyModule v
    writeStr na
    writeStr " "
    let b2 = hasLowerPrecThanInfixOp5 x2
    when b2 (writeStr "(") >> writePreTerm x2 >> when b2 (writeStr ")")
  | Str.isFixOp4 (varName v) = do
    let b1 = hasLowerPrecThanInfixOp4 x1
    when b1 (writeStr "(") >> writePreTerm x1 >> when b1 (writeStr ")")
    writeStr " "
    na <- simplifyModule v
    writeStr na
    writeStr " "
    let b2 = hasLowerPrecThanInfixOp3 x2
    when b2 (writeStr "(") >> writePreTerm x2 >> when b2 (writeStr ")")
  | Str.isFixOp3 (varName v) = do
    let b1 = hasLowerPrecThanInfixOp2 x1
    when b1 (writeStr "(") >> writePreTerm x1 >> when b1 (writeStr ")")
    writeStr " "
    na <- simplifyModule v
    writeStr na
    writeStr " "
    let b2 = hasLowerPrecThanInfixOp3 x2
    when b2 (writeStr "(") >> writePreTerm x2 >> when b2 (writeStr ")")
  | Str.isFixOp2 (varName v) = do
    let b1 = hasLowerPrecThanInfixOp2 x1
    when b1 (writeStr "(") >> writePreTerm x1 >> when b1 (writeStr ")")
    writeStr " "
    na <- simplifyModule v
    writeStr na
    writeStr " "
    let b2 = hasLowerPrecThanInfixOp1 x2
    when b2 (writeStr "(") >> writePreTerm x2 >> when b2 (writeStr ")")
  | Str.isFixOp1 (varName v) = do
    let b1 = hasLowerPrecThanPrefixOp x1
    when b1 (writeStr "(") >> writePreTerm x1 >> when b1 (writeStr ")")
    writeStr " "
    na <- simplifyModule v
    writeStr na
    writeStr " "
    let b2 = hasLowerPrecThanInfixOp1 x2
    when b2 (writeStr "(") >> writePreTerm x2 >> when b2 (writeStr ")")
  | True = error $ "expected infix operator to print, got " ++ varName v

writeNormalApp ::
  PreTerm -> [PreTerm] -> ToString ()
writeNormalApp f xs = do
  let b = hasLowerPrecThanApp f
  when b (writeStr "(")
  writePreTerm f
  when b (writeStr ")")
  if null xs
  then writeStr " ()"
  else do
    writeStr " "
    writePreTermList xs

writeCaseTree ::
  [Either VarName PreTerm] -> CaseTree -> ToString ()
writeCaseTree [] (CaseLeaf [] t _) =
  writePreTerm t
writeCaseTree (x : xs) (CaseLeaf (i : is) t _) = do
  unless (Str.isWildcard $ varName i) $ do
    writeStr (varName i)
    vb <- isVerbose
    when vb $ do
      writeStr "_"
      writeStr (show (varId i))
    writeStr " := "
    case x of
      Left n -> writeStr n
      Right e -> do
        when (hasSeqPrecOrLower e) (writeStr "(")
        writePreTerm e
        when (hasSeqPrecOrLower e) (writeStr ")")
    writeStr "; "
  writeCaseTree xs (CaseLeaf is t [])
writeCaseTree (_:_) (CaseLeaf [] _ _) =
  error "expected zero arguments to print case-expression"
writeCaseTree [] (CaseLeaf (_:_) _ _) =
  error "expected more than zero arguments to print case-expression"
writeCaseTree [] (CaseEmpty _) = error "empty case tree case without argument"
writeCaseTree ps (CaseEmpty idx) = do
  let (p, _) = removeIdx idx ps
  writeStr "case "
  case p of
    Left n -> writeStr n
    Right e -> writePreTerm e
  incIndent
  newLine
  writeStr "of {}"
  newLine
  writeStr "end"
  decIndent
writeCaseTree ps (CaseNode idx m d) = do
  let (p, ps') = removeIdx idx ps
  writeStr "case "
  case p of
    Left n -> writeStr n
    Right e -> writePreTerm e
  incIndent
  mapM_ (writeCase ps') (IntMap.toList m)
  case d of
    Nothing -> return ()
    Just (_vs, ct) -> do
      newLine
      writeStr "of _ => "
      writeCaseTree ps' ct
  newLine
  writeStr "end"
  decIndent
  where
    writeCase ::
      [Either VarName PreTerm] ->
      (VarId, ([Var], CaseTree)) -> ToString ()
    writeCase ps' (i, (_, ct)) = do
      newLine
      writeStr "of "
      rm <- liftTT Env.getRefMap
      im <- liftTT Env.getImplicitMap
      let t = Env.forceLookupRefMap i rm
      let imps = fromJust (IntMap.lookup i im)
      if not (null imps)
      then do
        let TermCtor v _ = termPre t
        writeStr (varName v)
        vb <- isVerbose
        when vb $ do
          writeStr "_"
          writeStr (show (varId v))
        when (not (null imps)) (writeStr " ")
        imps' <- writeImplicits imps
        names <- writeArgs (termTy t)
        writeStr " => "
        writeCaseTree (imps' ++ names ++ ps') ct
      else do
        (names, args) <- getArgs (termTy t)
        writePreTerm (applyArgs (termPre t) args)
        writeStr " => "
        writeCaseTree (names ++ ps') ct

    writeImplicits ::
      Implicits -> ToString [Either VarName PreTerm]
    writeImplicits [] = return []
    writeImplicits ((v, _) : vs) = do
      n <- nextVarName
      writeStr "["
      writeStr (varName v)
      vb <- isVerbose
      when vb $ do
        writeStr "_"
        writeStr (show (varId v))
      writeStr " := "
      writeStr n
      writeStr "]"
      when (not (null vs)) (writeStr " ")
      ns <- writeImplicits vs
      return (Left n : ns)

    writeArgs ::
      PreTerm -> ToString [Either VarName PreTerm]
    writeArgs ty = do
      rm <- liftTT Env.getRefMap
      case preTermDomCod rm ty of
        Nothing ->
          case preTermLazyCod rm ty of
            Nothing -> return []
            Just (c', _) -> do
              writeStr " []"
              writeArgs c'
        Just (d', c', _) -> do
          ns <- mapM (\_ -> nextVarName) [1 .. length d']
          when (not (null ns)) (writeStr " ")
          writeNameList ns
          nss <- writeArgs c'
          return (map Left ns ++ nss)

    getArgs ::
      PreTerm -> ToString ([Either VarName PreTerm], Maybe [Maybe [PreTerm]])
    getArgs ty = do
      rm <- liftTT Env.getRefMap
      case preTermDomCod rm ty of
        Nothing ->
          case preTermLazyCod rm ty of
            Nothing -> return ([], Nothing)
            Just (c', _) -> do
              (ns, as) <- getArgs c'
              case as of
                Nothing -> return (ns, Just [Nothing])
                Just as' -> return (ns, Just (Nothing : as'))
        Just (d', c', _) -> do
          vs <- mapM (\_ -> nextVarName) [1 .. length d']
          (ns, as) <- getArgs c'
          let ns' = map Left vs ++ ns
          let ws = map (\v -> TermVar False (mkVar (-1) v)) vs
          case as of
            Nothing -> return (ns', Just [Just ws])
            Just as' -> return (ns', Just (Just ws : as'))

    applyArgs :: PreTerm -> Maybe [Maybe [PreTerm]] -> PreTerm
    applyArgs t Nothing = t
    applyArgs t (Just (Nothing : as)) =
      applyArgs (TermLazyApp False t) (Just as)
    applyArgs t (Just (Just xs : as)) =
      applyArgs (TermApp False t xs) (Just as)
    applyArgs t (Just []) = t

writeCaseTree ps (CaseUnit idx (_vs, ct)) = do
  let (p, ps') = removeIdx idx ps
  writeStr "case "
  case p of
    Left n -> writeStr n
    Right e -> writePreTerm e
  incIndent
  newLine
  writeStr "of () => "
  writeCaseTree ps' ct
  newLine
  writeStr "end"
  decIndent

removeIdx :: Int -> [a] -> (a, [a])
removeIdx 0 (x:xs) = (x, xs)
removeIdx i (x:xs) = let (x', xs') = removeIdx (i-1) xs in (x', x : xs')
removeIdx _ [] = error "invalid index to remove"

writePreTermList ::
  [PreTerm] -> ToString ()
writePreTermList [] = return ()
writePreTermList [p] = do
  when (needsAppParens p) (writeStr "(")
  writePreTerm p
  when (needsAppParens p) (writeStr ")")
writePreTermList (p:ps) = do
  when (needsAppParens p) (writeStr "(")
  writePreTerm p
  when (needsAppParens p) (writeStr ")")
  writeStr " "
  writePreTermList ps

writeNamedPreTermArgs ::
  [(VarName, PreTerm)] -> ToString ()
writeNamedPreTermArgs [] = error "empty implicit application"
writeNamedPreTermArgs [(n, p)] = do
  writeStr "["
  writeStr n
  writeStr " := "
  writePreTerm p
  writeStr "]"
writeNamedPreTermArgs (p:ps) = do
  writeNamedPreTermArgs [p]
  writeStr " "
  writeNamedPreTermArgs ps

writeVarList :: [Var] -> ToString ()
writeVarList [] = return ()
writeVarList [v] = do
  writeStr (varName v)
  vb <- isVerbose
  when vb $ do
    writeStr "_"
    writeStr (show (varId v))
writeVarList (v:vs) = do
  writeStr (varName v)
  vb <- isVerbose
  when vb $ do
    writeStr "_"
    writeStr (show (varId v))
  writeStr " "
  writeVarList vs

writeNameList :: [VarName] -> ToString ()
writeNameList [] = return ()
writeNameList [v] = writeStr v
writeNameList (v:vs) =
  writeStr v >> writeStr " " >> writeNameList vs

writeDomainList ::
  [(Maybe Var, PreTerm)] -> ToString ()
writeDomainList [] = return ()
writeDomainList [v] = writeOptNamedPreTerm v
writeDomainList (v:vs) = do
  writeOptNamedPreTerm v
  writeStr " & "
  writeDomainList vs

writeOptNamedPreTerm ::
  (Maybe Var, PreTerm) -> ToString ()
writeOptNamedPreTerm (Nothing, t) = do
  when (hasLowerPrecThanInfixOp6 t) (writeStr "(")
  writePreTerm t
  when (hasLowerPrecThanInfixOp6 t) (writeStr ")")
writeOptNamedPreTerm (Just v, t) = do
  writeStr "("
  writeStr (varName v)
  vb <- isVerbose
  when vb $ do
    writeStr "_"
    writeStr (show (varId v))
  writeStr " : "
  writePreTerm t
  writeStr ")"

hasLowerPrecThanApp :: PreTerm -> Bool
hasLowerPrecThanApp (TermApp _ (TermVar _ v) [_])
  | Str.isFixOp (varName v) = True
  | True = False
hasLowerPrecThanApp (TermApp _ (TermCtor v _) [_])
  | Str.isFixOp (varName v) = True
  | True = False
hasLowerPrecThanApp (TermApp _ (TermData v) [_])
  | Str.isFixOp (varName v) = True
  | True = False
hasLowerPrecThanApp (TermApp _ (TermRef v _) [_])
  | Str.isFixOp (varName v) = True
  | True = False
hasLowerPrecThanApp (TermApp io (TermImplicitApp False t _) xs) =
  hasLowerPrecThanApp (TermApp io t xs)
hasLowerPrecThanApp t = hasLowerPrecThanPrefixOp t

hasLowerPrecThanPrefixOp :: PreTerm -> Bool
hasLowerPrecThanPrefixOp (TermApp _ (TermVar _ _) [_]) = False
hasLowerPrecThanPrefixOp t@(TermApp _ (TermVar _ v) [_, _])
  | Str.isFixOp1 (varName v) = True
  | True = hasLowerPrecThanInfixOp1 t
hasLowerPrecThanPrefixOp t@(TermApp _ (TermCtor v _) [_, _])
  | Str.isFixOp1 (varName v) = True
  | True = hasLowerPrecThanInfixOp1 t
hasLowerPrecThanPrefixOp t@(TermApp _ (TermData v) [_, _])
  | Str.isFixOp1 (varName v) = True
  | True = hasLowerPrecThanInfixOp1 t
hasLowerPrecThanPrefixOp t@(TermApp _ (TermRef v _) [_, _])
  | Str.isFixOp1 (varName v) = True
  | True = hasLowerPrecThanInfixOp1 t
hasLowerPrecThanPrefixOp (TermApp io (TermImplicitApp False t _) xs) =
  hasLowerPrecThanPrefixOp (TermApp io t xs)
hasLowerPrecThanPrefixOp t = hasLowerPrecThanInfixOp1 t

hasLowerPrecThanInfixOp1 :: PreTerm -> Bool
hasLowerPrecThanInfixOp1 t@(TermApp _ (TermVar _ v) [_, _])
  | Str.isFixOp2 (varName v) = True
  | True = hasLowerPrecThanInfixOp2 t
hasLowerPrecThanInfixOp1 t@(TermApp _ (TermCtor v _) [_, _])
  | Str.isFixOp2 (varName v) = True
  | True = hasLowerPrecThanInfixOp2 t
hasLowerPrecThanInfixOp1 t@(TermApp _ (TermData v) [_, _])
  | Str.isFixOp2 (varName v) = True
  | True = hasLowerPrecThanInfixOp2 t
hasLowerPrecThanInfixOp1 t@(TermApp _ (TermRef v _) [_, _])
  | Str.isFixOp2 (varName v) = True
  | True = hasLowerPrecThanInfixOp2 t
hasLowerPrecThanInfixOp1 (TermApp io (TermImplicitApp False t _) xs) =
  hasLowerPrecThanInfixOp1 (TermApp io t xs)
hasLowerPrecThanInfixOp1 t = hasLowerPrecThanInfixOp2 t

hasLowerPrecThanInfixOp2 :: PreTerm -> Bool
hasLowerPrecThanInfixOp2 t@(TermApp _ (TermVar _ v) [_, _])
  | Str.isFixOp3 (varName v) = True
  | True = hasLowerPrecThanInfixOp3 t
hasLowerPrecThanInfixOp2 t@(TermApp _ (TermCtor v _) [_, _])
  | Str.isFixOp3 (varName v) = True
  | True = hasLowerPrecThanInfixOp3 t
hasLowerPrecThanInfixOp2 t@(TermApp _ (TermRef v _) [_, _])
  | Str.isFixOp3 (varName v) = True
  | True = hasLowerPrecThanInfixOp3 t
hasLowerPrecThanInfixOp2 t@(TermApp _ (TermData v) [_, _])
  | Str.isFixOp3 (varName v) = True
  | True = hasLowerPrecThanInfixOp3 t
hasLowerPrecThanInfixOp2 (TermApp io (TermImplicitApp False t _) xs) =
  hasLowerPrecThanInfixOp2 (TermApp io t xs)
hasLowerPrecThanInfixOp2 t = hasLowerPrecThanInfixOp3 t

hasLowerPrecThanInfixOp3 :: PreTerm -> Bool
hasLowerPrecThanInfixOp3 t@(TermApp _ (TermVar _ v) [_, _])
  | Str.isFixOp4 (varName v) = True
  | True = hasLowerPrecThanInfixOp4 t
hasLowerPrecThanInfixOp3 t@(TermApp _ (TermCtor v _) [_, _])
  | Str.isFixOp4 (varName v) = True
  | True = hasLowerPrecThanInfixOp4 t
hasLowerPrecThanInfixOp3 t@(TermApp _ (TermData v) [_, _])
  | Str.isFixOp4 (varName v) = True
  | True = hasLowerPrecThanInfixOp4 t
hasLowerPrecThanInfixOp3 t@(TermApp _ (TermRef v _) [_, _])
  | Str.isFixOp4 (varName v) = True
  | True = hasLowerPrecThanInfixOp4 t
hasLowerPrecThanInfixOp3 (TermApp io (TermImplicitApp False t _) xs) =
  hasLowerPrecThanInfixOp3 (TermApp io t xs)
hasLowerPrecThanInfixOp3 t = hasLowerPrecThanInfixOp4 t

hasLowerPrecThanInfixOp4 :: PreTerm -> Bool
hasLowerPrecThanInfixOp4 t@(TermApp _ (TermVar _ v) [_, _])
  | Str.isFixOp5 (varName v) = True
  | True = hasLowerPrecThanInfixOp5 t
hasLowerPrecThanInfixOp4 t@(TermApp _ (TermCtor v _) [_, _])
  | Str.isFixOp5 (varName v) = True
  | True = hasLowerPrecThanInfixOp5 t
hasLowerPrecThanInfixOp4 t@(TermApp _ (TermData v) [_, _])
  | Str.isFixOp5 (varName v) = True
  | True = hasLowerPrecThanInfixOp5 t
hasLowerPrecThanInfixOp4 t@(TermApp _ (TermRef v _) [_, _])
  | Str.isFixOp5 (varName v) = True
  | True = hasLowerPrecThanInfixOp5 t
hasLowerPrecThanInfixOp4 (TermApp io (TermImplicitApp False t _) xs) =
  hasLowerPrecThanInfixOp4 (TermApp io t xs)
hasLowerPrecThanInfixOp4 t = hasLowerPrecThanInfixOp5 t

hasLowerPrecThanInfixOp5 :: PreTerm -> Bool
hasLowerPrecThanInfixOp5 t@(TermApp _ (TermVar _ v) [_, _])
  | Str.isFixOp6 (varName v) = True
  | True = hasLowerPrecThanInfixOp6 t
hasLowerPrecThanInfixOp5 t@(TermApp _ (TermCtor v _) [_, _])
  | Str.isFixOp6 (varName v) = True
  | True = hasLowerPrecThanInfixOp6 t
hasLowerPrecThanInfixOp5 t@(TermApp _ (TermData v) [_, _])
  | Str.isFixOp6 (varName v) = True
  | True = hasLowerPrecThanInfixOp6 t
hasLowerPrecThanInfixOp5 t@(TermApp _ (TermRef v _) [_, _])
  | Str.isFixOp6 (varName v) = True
  | True = hasLowerPrecThanInfixOp6 t
hasLowerPrecThanInfixOp5 (TermApp io (TermImplicitApp False t _) xs) =
  hasLowerPrecThanInfixOp5 (TermApp io t xs)
hasLowerPrecThanInfixOp5 t = hasLowerPrecThanInfixOp6 t

hasLowerPrecThanInfixOp6 :: PreTerm -> Bool
hasLowerPrecThanInfixOp6 (TermLazyArrow _ _) = True
hasLowerPrecThanInfixOp6 (TermArrow _ _ _) = True
hasLowerPrecThanInfixOp6 t = hasSeqPrecOrLower t

hasSeqPrecOrLower :: PreTerm -> Bool
hasSeqPrecOrLower (TermCase _ (CaseLeaf _ _ _)) = True
hasSeqPrecOrLower t = hasFunPrecOrLower t

hasFunPrecOrLower :: PreTerm -> Bool
hasFunPrecOrLower (TermFun _ _ _ _) = True
hasFunPrecOrLower (TermImplicitApp False (TermFun _ _ _ _) _) = True
hasFunPrecOrLower (TermLazyFun _ _) = True
hasFunPrecOrLower _ = False

needsAppParens :: PreTerm -> Bool
needsAppParens (TermRef _ _) = False
needsAppParens (TermData _) = False
needsAppParens (TermCtor _ _) = False
needsAppParens (TermVar _ _) = False
needsAppParens TermUnitElem = False
needsAppParens TermUnitTy = False
needsAppParens TermTy = False
needsAppParens TermEmpty = False
needsAppParens (TermCase _ (CaseLeaf _ _ _)) = True
needsAppParens (TermCase _ _) = False
needsAppParens (TermImplicitApp False t _) = needsAppParens t
needsAppParens _ = True
