{-# LANGUAGE BangPatterns #-}

module TypeCheck.TermEnv
  ( preTermVars
  , preTermRefs
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
  , patternProjArgs
  , patternApply
  , prePatternApplyWithVars
  , preTermProjArgs
  , preTermToString
  , prePatternToString
  , caseTreeToString
  )
where

import TypeCheck.TypeCheckT
import TypeCheck.SubstMap
import TypeCheck.Term
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified TypeCheck.Env as Env
import Data.Maybe (fromJust, isJust, isNothing)
import Control.Exception (assert)
import Control.Monad.Trans.State
import Control.Monad.Writer
--import Data.List (sortOn, intercalate)
import Data.List (sortOn)
import qualified Str

import Debug.Trace (trace)

_useTrace :: String -> a -> a
_useTrace = trace

caseTreeVars :: IntSet -> RefMap -> CaseTree -> VarIdSet
caseTreeVars vis rm = caseVars
  where
    caseVars :: CaseTree -> VarIdSet
    caseVars (CaseLeaf is _ t _) =
      let t' = doPreTermVars vis rm t
      in IntSet.difference t' (IntSet.fromList (map varId is))
    caseVars (CaseEmpty _) = IntSet.empty
    caseVars (CaseNode _ m d) =
      let i1 = foldl addCaseTree IntSet.empty m
      in case d of
          Nothing -> i1
          Just d' ->
            let a = addCaseTree IntSet.empty d'
            in IntSet.union i1 a
    caseVars (CaseUnit _ d) = addCaseTree IntSet.empty d

    addCaseTree :: VarIdSet -> ([Var], CaseTree) -> VarIdSet
    addCaseTree s (i, t) =
      let j = caseVars t
          i' = IntSet.difference j (IntSet.fromList (map varId i))
      in IntSet.union i' s

preTermVars :: RefMap -> PreTerm -> VarIdSet
preTermVars r t = doPreTermVars IntSet.empty r t

doPreTermVars :: IntSet -> RefMap -> PreTerm -> VarIdSet
doPreTermVars vis r (TermFun _ _ _ caseTree) = caseTreeVars vis r caseTree
doPreTermVars vis r (TermLazyFun _ t) = doPreTermVars vis r t
doPreTermVars vis r (TermArrow _ as t) =
  let f s (_, i) = IntSet.union s (doPreTermVars vis r i)
      as' = foldl f IntSet.empty as
      a' = doPreTermVars vis r t
  in IntSet.union a' as'
doPreTermVars vis r (TermLazyArrow _ t) = doPreTermVars vis r t
doPreTermVars vis r (TermApp _ a as) =
  let a' = doPreTermVars vis r a
      f s i = IntSet.union s (doPreTermVars vis r i)
      as' = foldl f IntSet.empty as
  in IntSet.union a' as'
doPreTermVars vis r (TermImplicitApp _ a as) =
  let a' = doPreTermVars vis r a
      f s i = IntSet.union s (doPreTermVars vis r (snd i))
      as' = foldl f IntSet.empty as
  in IntSet.union a' as'
doPreTermVars vis r (TermLazyApp _ t) = doPreTermVars vis r t
doPreTermVars vis r (TermRef v su) = do
  if IntSet.member (varId v) vis
    then IntSet.empty
    else let vis' = IntSet.insert (varId v) vis
             t0 = IntMap.lookup (varId v) r
         in case t0 of
              Nothing -> IntSet.empty
              Just (t, _) -> doPreTermVars vis' r (substPreTerm su (termPre t))
doPreTermVars _ _ (TermVar _ v) = IntSet.singleton (varId v)
doPreTermVars _ _ (TermData _) = IntSet.empty
doPreTermVars _ _ (TermCtor _ _) = IntSet.empty
doPreTermVars vis r (TermCase t ct) =
  let t' = doPreTermVars vis r t
      ct' = caseTreeVars vis r ct
  in IntSet.union t' ct'
doPreTermVars _ _ TermUnitElem = IntSet.empty
doPreTermVars _ _ TermUnitTy = IntSet.empty
doPreTermVars _ _ TermEmpty = IntSet.empty
doPreTermVars _ _ TermTy = IntSet.empty

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
    caseRefs (CaseLeaf _ _ t _) = doPreTermRefs vis rm t
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
                let s = doPreTermRefs vis' r (substPreTerm su (termPre t))
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
        && doPreTermsAlphaEqual bi r a1 (substPreTerm su a2)
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
      in (Just v', substPreTerm su t)
    substArrow su (Nothing, t) = (Nothing, substPreTerm su t)
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
            r1' = substPreTerm s1 (termPre r1)
            r2' = substPreTerm s2 (termPre r2)
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
      r1' = substPreTerm s1 (termPre r1)
  in doPreTermsAlphaEqual bi r r1' t2
doPreTermsAlphaEqual bi r t1 (TermRef v2 s2)
  | isNothing (IntMap.lookup (varId v2) r) = False
  | True =
  let r2 = Env.forceLookupRefMap (varId v2) r
      r2' = substPreTerm s2 (termPre r2)
  in doPreTermsAlphaEqual bi r t1 r2'
doPreTermsAlphaEqual bi r (TermLazyFun io t1) t2 =
  doPreTermsAlphaEqual bi r t1 (TermLazyApp io t2)
doPreTermsAlphaEqual bi r t1 (TermLazyFun io t2) =
  doPreTermsAlphaEqual bi r (TermLazyApp io t1) t2
doPreTermsAlphaEqual bi r (TermFun [] io1 _ (CaseLeaf i1 _ t1 _)) t2 =
  doPreTermsAlphaEqual bi r
    t1 (TermApp io1 t2 (map (\i -> TermVar False (mkVar (varId i) "_")) i1))
doPreTermsAlphaEqual bi r t1 (TermFun [] io2 _ (CaseLeaf i2 _ t2 _)) =
  doPreTermsAlphaEqual bi r
    (TermApp io2 t1 (map (\i -> TermVar False (mkVar (varId i) "_")) i2)) t2
doPreTermsAlphaEqual bi r (TermCase t1 ct1) (TermCase t2 ct2) =
  doPreTermsAlphaEqual bi r t1 t2
  && caseTreesAlphaEqual bi IntMap.empty r ct1 ct2
doPreTermsAlphaEqual _ _ _ _ = False

caseTreesAlphaEqual ::
  RefBisim -> SubstMap -> RefMap -> CaseTree -> CaseTree -> Bool
caseTreesAlphaEqual bi subst rm (CaseLeaf i1 io1 t1 _) (CaseLeaf i2 io2 t2 _) = do
  io1 == io2
  && length i1 == length i2
  && let su0 = if map varId i1 /= map varId i2
                then IntMap.fromList (zip (map varId i2) (map (TermVar False) i1))
                else IntMap.empty
         su = IntMap.union subst su0
     in doPreTermsAlphaEqual bi rm t1 (substPreTerm su t2)
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
preTermNormalize m (TermFun is False n (CaseLeaf vs io t ws)) =
  let !() = assert (not io) ()
  in TermFun is False n (CaseLeaf vs False (preTermNormalize m t) ws)
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
        let t' = substPreTerm s (termPre t)
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
      TermFun _ io n (CaseLeaf is io' te ws) ->
        let z = zip (map varId is) (map snd xs')
            is' = drop (length z) is
            te' = substPreTerm (IntMap.fromList z) te
        in TermFun [] io n (CaseLeaf is' io' te' ws)
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
doCaseTreeNormalize _ subst (CaseLeaf is _ te _) xs =
  let s = assert (length xs == length is) (zip (map varId is) xs)
      subst' = IntMap.union subst (IntMap.fromList s)
  in Just (substPreTerm subst' te)
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
-- Bool = True if lazy arrow on the way.
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
preTermDomCod = \r t -> doPreTermDomCod (preTermNormalize r t)
  where
    doPreTermDomCod ::
      PreTerm -> Maybe ([(Maybe Var, PreTerm)], PreTerm, Bool)
    doPreTermDomCod (TermArrow io d c) = Just (d, c, io)
    doPreTermDomCod _ = Nothing

preTermLazyCod :: RefMap -> PreTerm -> Maybe (PreTerm, Bool)
preTermLazyCod = \r t -> doPreTermLazyCod (preTermNormalize r t)
  where
    doPreTermLazyCod :: PreTerm -> Maybe (PreTerm, Bool)
    doPreTermLazyCod (TermLazyArrow io c) = Just (c, io)
    doPreTermLazyCod _ = Nothing

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

prePatternApplyWithVars :: Monad m =>
  PrePattern -> PreTerm -> TypeCheckT m Pattern
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
    addArg :: Monad m => (Var, PreTerm) -> TypeCheckT m (VarName, PrePattern)
    addArg (n, _) = do
      i <- Env.freshVarId
      let v = mkVar i "_"
      let a = PatternVar v
      return (varName n, a)

    addArg1 :: Monad m => (Maybe Var, PreTerm) -> TypeCheckT m PrePattern
    addArg1 _ = do
      i <- Env.freshVarId
      let v = mkVar i "_"
      let a = PatternVar v
      return a

    getVars :: Monad m => PreTerm -> TypeCheckT m [Maybe [PrePattern]]
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
              c = substPreTerm su ty
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
              c = substPreTerm su c'
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
                                 patternTy = substPreTerm su d}) xs
      in (substPreTerm su ty, (zip (map fst as) as', ps))
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
                                 patternTy = substPreTerm su d}) xs
      in (substPreTerm su c, (b, Just as' : ps))
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

----------------- Pretty printing -------------------------------

preTermToString :: ImplicitMap -> RefMap -> Int -> PreTerm -> String
preTermToString i r indent t =
  execWriter (runStateT (writeIndent >> writePreTerm i r t) (1, indent))

prePatternToString :: ImplicitMap -> RefMap -> Int -> PrePattern -> String
prePatternToString im rm indent t =
  preTermToString im rm indent (prePatternToPreTerm t)

caseTreeToString :: ImplicitMap -> RefMap -> Int -> CaseTree -> [PreTerm] -> String
caseTreeToString i r indent t as =
  execWriter (runStateT (writeIndent >> writeCaseTree i r (map Right as) t) (1, indent))

type ToString a = StateT (Int, Int) (Writer String) a

incIndent :: ToString ()
incIndent = modify (\(x, n) -> (x, n+2))

decIndent :: ToString ()
decIndent = modify (\(x, n) -> (x, n-2))

nextVarName :: ToString String
nextVarName = do
  (x, n) <- get
  put (x+1, n)
  return ('.' : show x)

writeIndent :: ToString ()
writeIndent = get >>= doWrite . snd
  where
    doWrite :: Int -> ToString ()
    doWrite 0 = return ()
    doWrite i = tell " " >> doWrite (i - 1)

writeStr :: String -> ToString ()
writeStr s = tell s

newLine :: ToString ()
newLine = tell "\n" >> writeIndent

writePreTerm :: ImplicitMap -> RefMap -> PreTerm -> ToString ()
writePreTerm im rm (TermFun imps _ (Just _) (CaseLeaf vs _ t _)) = do
  writeStr "("
  writeVarList (drop (length imps) vs)
  writeStr "). "
  writePreTerm im rm t
writePreTerm im rm (TermFun imps _ Nothing ct) =
  writeCaseTree im rm (map Left imps) ct
writePreTerm im rm (TermFun imps _ (Just n) ct) = do
  args <- mapM (\_ -> nextVarName) [1..n]
  writeStr "("
  writeNameList args
  writeStr "). "
  writeCaseTree im rm (map Left imps ++ map Left args) ct
writePreTerm im rm (TermLazyFun _ f) =
  writeStr "[]. " >> writePreTerm im rm f
writePreTerm im rm (TermArrow io d c) = do
  optNamedListWithParens rm d (writeOptNamedPreTermList im rm d)
  if io
    then writeStr " ->> "
    else writeStr " -> "
  writePreTerm im rm c
writePreTerm im rm (TermLazyArrow io t) = do
  if io
    then writeStr "[] ->> "
    else writeStr "[] -> "
  writePreTerm im rm t
writePreTerm im rm (TermApp _ f@(TermVar _ v) [x])
  | Str.isPrefixOp (varName v) =
      writeUnaryApp im rm (varName v) f x
writePreTerm im rm
    (TermApp _ (TermImplicitApp False f@(TermVar _ v) _) [x])
  | Str.isPrefixOp (varName v) =
      writeUnaryApp im rm (varName v) f x
writePreTerm im rm (TermApp _ f@(TermCtor v _) [x])
  | Str.isPrefixOp (varName v) =
      writeUnaryApp im rm (varName v) f x
writePreTerm im rm
    (TermApp _ (TermImplicitApp False f@(TermCtor v _) _) [x])
  | Str.isPrefixOp (varName v) =
      writeUnaryApp im rm (varName v) f x
writePreTerm im rm (TermApp _ f@(TermData v) [x])
  | Str.isPrefixOp (varName v) =
      writeUnaryApp im rm (varName v) f x
writePreTerm im rm
    (TermApp _ (TermImplicitApp False f@(TermData v) _) [x])
  | Str.isPrefixOp (varName v) =
      writeUnaryApp im rm (varName v) f x
writePreTerm im rm (TermApp _ f@(TermRef v _) [x])
  | Str.isPrefixOp (varName v) =
      writeUnaryApp im rm (varName v) f x
writePreTerm im rm
    (TermApp _ (TermImplicitApp False f@(TermRef v _) _) [x])
  | Str.isPrefixOp (varName v) =
      writeUnaryApp im rm (varName v) f x
writePreTerm im rm (TermApp _ f@(TermVar _ v) [x1, x2])
  | Str.isInfixOp (varName v) =
      writeBinaryApp im rm (varName v) f x1 x2
writePreTerm im rm
    (TermApp _ (TermImplicitApp False f@(TermVar _ v) _) [x1, x2])
  | Str.isInfixOp (varName v) =
      writeBinaryApp im rm (varName v) f x1 x2
writePreTerm im rm (TermApp _ f@(TermCtor v _) [x1, x2])
  | Str.isInfixOp (varName v) =
      writeBinaryApp im rm (varName v) f x1 x2
writePreTerm im rm
    (TermApp _ (TermImplicitApp False f@(TermCtor v _) _) [x1, x2])
  | Str.isInfixOp (varName v) =
      writeBinaryApp im rm (varName v) f x1 x2
writePreTerm im rm (TermApp _ f@(TermData v) [x1, x2])
  | Str.isInfixOp (varName v) =
      writeBinaryApp im rm (varName v) f x1 x2
writePreTerm im rm
    (TermApp _ (TermImplicitApp False f@(TermData v) _) [x1, x2])
  | Str.isInfixOp (varName v) =
      writeBinaryApp im rm (varName v) f x1 x2
writePreTerm im rm (TermApp _ f@(TermRef v _) [x1, x2])
  | Str.isInfixOp (varName v) =
      writeBinaryApp im rm (varName v) f x1 x2
writePreTerm im rm
    (TermApp _ (TermImplicitApp False f@(TermRef v _) _) [x1, x2])
  | Str.isInfixOp (varName v) =
      writeBinaryApp im rm (varName v) f x1 x2
writePreTerm im rm (TermApp _ f@(TermVar _ v) (x:xs))
  | Str.isPostfixOp (varName v) =
      writePostfixApp im rm (varName v) f x xs
writePreTerm im rm
    (TermApp _ (TermImplicitApp False f@(TermVar _ v) _) (x:xs))
  | Str.isPostfixOp (varName v) =
      writePostfixApp im rm (varName v) f x xs
writePreTerm im rm (TermApp _ f@(TermCtor v _) (x:xs))
  | Str.isPostfixOp (varName v) =
      writePostfixApp im rm (varName v) f x xs
writePreTerm im rm
    (TermApp _ (TermImplicitApp False f@(TermCtor v _) _) (x:xs))
  | Str.isPostfixOp (varName v) =
      writePostfixApp im rm (varName v) f x xs
writePreTerm im rm (TermApp _ f@(TermData v) (x:xs))
  | Str.isPostfixOp (varName v) =
      writePostfixApp im rm (varName v) f x xs
writePreTerm im rm
    (TermApp _ (TermImplicitApp False f@(TermData v) _) (x:xs))
  | Str.isPostfixOp (varName v) =
      writePostfixApp im rm (varName v) f x xs
writePreTerm im rm (TermApp _ f@(TermRef v _) (x:xs))
  | Str.isPostfixOp (varName v) =
      writePostfixApp im rm (varName v) f x xs
writePreTerm im rm
    (TermApp _ (TermImplicitApp False f@(TermRef v _) _) (x:xs))
  | Str.isPostfixOp (varName v) =
      writePostfixApp im rm (varName v) f x xs
writePreTerm im rm (TermApp _ f xs) = writeNormalApp im rm f xs
writePreTerm im rm (TermImplicitApp True f xs) = do
  let b = hasLowerPrecThanApp f
  when b (writeStr "(")
  writePreTerm im rm f
  when b (writeStr ")")
  writeStr "["
  writeNamedPreTermArgs im rm xs
  writeStr "]"
writePreTerm im rm (TermImplicitApp False f _) =
  writePreTerm im rm f
  --writePreTerm im rm (TermImplicitApp True f xs)
writePreTerm im rm (TermLazyApp _ f) = do
  let b = hasLowerPrecThanApp f
  when b (writeStr "(")
  writePreTerm im rm f
  when b (writeStr ")")
  writeStr "[]"
writePreTerm _ _ (TermRef v _) = writeStr (varName v)
writePreTerm _ _ (TermData v) = writeStr (varName v)
writePreTerm _ _ (TermCtor v _) = writeStr (varName v)
writePreTerm _ _ (TermVar _ v) = do
  let pre = Str.isPrefixOp (varName v)
  when pre (writeStr "(")
  writeStr (varName v)
  when pre (writeStr ")")
  --writeStr "."
  --writeStr (show (varId v))
writePreTerm _ _ TermUnitElem = writeStr "[]"
writePreTerm _ _ TermUnitTy = writeStr "{}"
writePreTerm _ _ TermTy = writeStr "Ty"
writePreTerm _ _ TermEmpty = writeStr "()"
writePreTerm im rm (TermCase e ct) = writeCaseTree im rm [Right e] ct

takeOperatorStr :: String -> String
takeOperatorStr = Str.stripOperatorStr . fst . Str.operandSplit

writePostfixApp ::
  ImplicitMap -> RefMap -> String ->
  PreTerm -> PreTerm -> [PreTerm] -> ToString ()
writePostfixApp im rm na _f x xs = do
  let b = hasLowerPrecThanApp x
  when b (writeStr "(")
  writePreTerm im rm x
  when b (writeStr ")")
  writeStr (' ' : takeOperatorStr na)
  when (not (null xs)) $ do
    writeStr "("
    writePreTermList im rm xs
    writeStr ")"

writeUnaryApp ::
  ImplicitMap -> RefMap -> String -> PreTerm -> PreTerm -> ToString ()
writeUnaryApp im rm na _f x = do
  writeStr (takeOperatorStr na) >> writeStr " "
  let b = hasLowerPrecThanPrefixOp x
  when b (writeStr "(")
  writePreTerm im rm x
  when b (writeStr ")")

writeBinaryApp ::
  ImplicitMap -> RefMap -> String ->
  PreTerm -> PreTerm -> PreTerm -> ToString ()
writeBinaryApp im rm na _f x1 x2
  | Str.isInfixOp6 na = do
    let b1 = hasLowerPrecThanInfixOp6 x1
    when b1 (writeStr "(") >> writePreTerm im rm x1 >> when b1 (writeStr ")")
    writeStr " " >> writeStr (takeOperatorStr na) >> writeStr " "
    let b2 = hasLowerPrecThanInfixOp5 x2
    when b2 (writeStr "(") >> writePreTerm im rm x2 >> when b2 (writeStr ")")
  | Str.isInfixOp5 na = do
    let b1 = hasLowerPrecThanInfixOp4 x1
    when b1 (writeStr "(") >> writePreTerm im rm x1 >> when b1 (writeStr ")")
    writeStr " " >> writeStr (takeOperatorStr na) >> writeStr " "
    let b2 = hasLowerPrecThanInfixOp5 x2
    when b2 (writeStr "(") >> writePreTerm im rm x2 >> when b2 (writeStr ")")
  | Str.isInfixOp4 na = do
    let b1 = hasLowerPrecThanInfixOp4 x1
    when b1 (writeStr "(") >> writePreTerm im rm x1 >> when b1 (writeStr ")")
    writeStr " " >> writeStr (takeOperatorStr na) >> writeStr " "
    let b2 = hasLowerPrecThanInfixOp3 x2
    when b2 (writeStr "(") >> writePreTerm im rm x2 >> when b2 (writeStr ")")
  | Str.isInfixOp3 na = do
    let b1 = hasLowerPrecThanInfixOp2 x1
    when b1 (writeStr "(") >> writePreTerm im rm x1 >> when b1 (writeStr ")")
    writeStr " " >> writeStr (takeOperatorStr na) >> writeStr " "
    let b2 = hasLowerPrecThanInfixOp3 x2
    when b2 (writeStr "(") >> writePreTerm im rm x2 >> when b2 (writeStr ")")
  | Str.isInfixOp2 na = do
    let b1 = hasLowerPrecThanInfixOp2 x1
    when b1 (writeStr "(") >> writePreTerm im rm x1 >> when b1 (writeStr ")")
    writeStr " " >> writeStr (takeOperatorStr na) >> writeStr " "
    let b2 = hasLowerPrecThanInfixOp1 x2
    when b2 (writeStr "(") >> writePreTerm im rm x2 >> when b2 (writeStr ")")
  | Str.isInfixOp1 na = do
    let b1 = hasLowerPrecThanPrefixOp x1
    when b1 (writeStr "(") >> writePreTerm im rm x1 >> when b1 (writeStr ")")
    writeStr " " >> writeStr (takeOperatorStr na) >> writeStr " "
    let b2 = hasLowerPrecThanInfixOp1 x2
    when b2 (writeStr "(") >> writePreTerm im rm x2 >> when b2 (writeStr ")")
  | True = error $ "expected infix operator to print, got " ++ na

writeNormalApp ::
  ImplicitMap -> RefMap -> PreTerm -> [PreTerm] -> ToString ()
writeNormalApp im rm f xs = do
  let b = hasLowerPrecThanApp f
  when b (writeStr "(")
  writePreTerm im rm f
  when b (writeStr ")")
  writeStr "("
  writePreTermList im rm xs
  writeStr ")"

writeCaseTree ::
  ImplicitMap -> RefMap -> [Either VarName PreTerm] -> CaseTree -> ToString ()
writeCaseTree im rm [] (CaseLeaf [] _ t _) = writePreTerm im rm t
writeCaseTree im rm (x : xs) (CaseLeaf (i : is) _ t _) = do
  when (varName i /= "_") $ do
    writeStr (varName i {- ++ "." ++ show (varId i) -})
    writeStr " := "
    case x of
      Left n -> writeStr n
      Right e -> writePreTerm im rm e
    writeStr "; "
  writeCaseTree im rm xs (CaseLeaf is False t [])
writeCaseTree _ _ (_:_) (CaseLeaf [] _ _ _) =
  error "expected zero arguments to print case-expression"
writeCaseTree _ _ [] (CaseLeaf (_:_) _ _ _) =
  error "expected more than zero arguments to print case-expression"
writeCaseTree _ _ [] (CaseEmpty _) = error "empty case tree case without argument"
writeCaseTree im rm ps (CaseEmpty idx) = do
  let (p, _) = removeIdx idx ps
  writeStr "case "
  case p of
    Left n -> writeStr n
    Right e -> writePreTerm im rm e
  incIndent
  newLine
  writeStr "of ()"
  newLine
  writeStr "end"
  decIndent
writeCaseTree im rm ps (CaseNode idx m d) = do
  let (p, ps') = removeIdx idx ps
  writeStr "case "
  case p of
    Left n -> writeStr n
    Right e -> writePreTerm im rm e
  incIndent
  mapM_ (writeCase ps') (IntMap.toList m)
  case d of
    Nothing -> return ()
    Just (_vs, ct) -> do
      newLine
      writeStr "of _ => "
      writeCaseTree im rm ps' ct
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
      let t = Env.forceLookupRefMap i rm
      let imps = fromJust (IntMap.lookup i im)
      if not (null imps)
      then do
        let TermCtor v _ = termPre t
        writeStr (varName v {- ++ "." ++ show (varId v) -})
        writeStr "["
        imps' <- writeImplicits imps
        writeStr "]"
        names <- writeArgs (termTy t)
        writeStr " => "
        writeCaseTree im rm (imps' ++ names ++ ps') ct
      else do
        (names, args) <- getArgs (termTy t)
        writePreTerm im rm (applyArgs (termPre t) args)
        writeStr " => "
        writeCaseTree im rm (names ++ ps') ct

    writeImplicits :: Implicits -> ToString [Either VarName PreTerm]
    writeImplicits [] = return []
    writeImplicits ((v, _) : vs) = do
      n <- nextVarName
      writeStr (varName v {- ++ "." ++ show (varId v) -})
      writeStr " := "
      writeStr n
      when (not (null vs)) (writeStr ", ")
      ns <- writeImplicits vs
      return (Left n : ns)

    writeArgs :: PreTerm -> ToString [Either VarName PreTerm]
    writeArgs ty =
      case preTermDomCod rm ty of
        Nothing ->
          case preTermLazyCod rm ty of
            Nothing -> return []
            Just (c', _) -> do
              writeStr "[]"
              writeArgs c'
        Just (d', c', _) -> do
          writeStr "("
          ns <- mapM (\_ -> nextVarName) [1 .. length d']
          writeNameList ns
          writeStr ")"
          nss <- writeArgs c'
          return (map Left ns ++ nss)

    getArgs ::
      PreTerm -> ToString ([Either VarName PreTerm], Maybe [Maybe [PreTerm]])
    getArgs ty = do
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

writeCaseTree im rm ps (CaseUnit idx (_vs, ct)) = do
  let (p, ps') = removeIdx idx ps
  writeStr "case "
  case p of
    Left n -> writeStr n
    Right e -> writePreTerm im rm e
  incIndent
  newLine
  writeStr "of [] => "
  writeCaseTree im rm ps' ct
  newLine
  writeStr "end"
  decIndent

removeIdx :: Int -> [a] -> (a, [a])
removeIdx 0 (x:xs) = (x, xs)
removeIdx i (x:xs) = let (x', xs') = removeIdx (i-1) xs in (x', x : xs')
removeIdx _ [] = error "invalid index to remove"

writePreTermList :: ImplicitMap -> RefMap -> [PreTerm] -> ToString ()
writePreTermList _ _ [] = return ()
writePreTermList im rm [p] = writePreTerm im rm p
writePreTermList im rm (p:ps) = do
  writePreTerm im rm p
  writeStr ", "
  writePreTermList im rm ps

writeNamedPreTermArgs ::
  ImplicitMap -> RefMap -> [(VarName, PreTerm)] -> ToString ()
writeNamedPreTermArgs _ _ [] = error "empty implicit application"
writeNamedPreTermArgs im rm [(n, p)] = do
  writeStr n
  writeStr " := "
  writePreTerm im rm p
writeNamedPreTermArgs im rm (p:ps) = do
  writeNamedPreTermArgs im rm [p]
  writeStr ", "
  writeNamedPreTermArgs im rm ps

writeVarList :: [Var] -> ToString ()
writeVarList [] = return ()
writeVarList [v] = writeStr (varName v)
writeVarList (v:vs) =
  writeStr (varName v) >> writeStr ", " >> writeVarList vs

writeNameList :: [VarName] -> ToString ()
writeNameList [] = return ()
writeNameList [v] = writeStr v
writeNameList (v:vs) =
  writeStr v >> writeStr ", " >> writeNameList vs

optNamedListWithParens ::
  RefMap -> [(Maybe Var, PreTerm)] -> ToString () -> ToString ()
optNamedListWithParens _ [] s = writeStr "(" >> s >> writeStr ")"
optNamedListWithParens rm [(v, ty)] s =
  if isJust v || needParen then writeStr "(" >> s >> writeStr ")" else s
  where
    needParen :: Bool
    needParen = case preTermNormalize rm ty of
                  TermArrow _ _ _ -> True
                  TermLazyArrow _ _ -> True
                  TermFun _ _ _ _ -> True
                  TermLazyFun _ _ -> True
                  _ -> False
optNamedListWithParens _ (_:_) s = writeStr "(" >> s >> writeStr ")"

writeOptNamedPreTermList ::
  ImplicitMap -> RefMap -> [(Maybe Var, PreTerm)] -> ToString ()
writeOptNamedPreTermList _ _ [] = return ()
writeOptNamedPreTermList im rm [v] = writeOptNamedPreTerm im rm v
writeOptNamedPreTermList im rm (v:vs) = do
  writeOptNamedPreTerm im rm v
  writeStr ", "
  writeOptNamedPreTermList im rm vs

writeOptNamedPreTerm ::
  ImplicitMap -> RefMap -> (Maybe Var, PreTerm) -> ToString ()
writeOptNamedPreTerm im rm (Nothing, t) = writePreTerm im rm t
writeOptNamedPreTerm im rm (Just v, t) = do
  writeStr (varName v)
  --writeStr "."
  --writeStr (show (varId v))
  writeStr " : "
  writePreTerm im rm t

hasLowerPrecThanApp :: PreTerm -> Bool
hasLowerPrecThanApp (TermApp _ (TermVar _ v) [_])
  | Str.isPrefixOp (varName v) = True
  | True = False
hasLowerPrecThanApp (TermApp _ (TermCtor v _) [_])
  | Str.isPrefixOp (varName v) = True
  | True = False
hasLowerPrecThanApp (TermApp _ (TermData v) [_])
  | Str.isPrefixOp (varName v) = True
  | True = False
hasLowerPrecThanApp (TermApp _ (TermRef v _) [_])
  | Str.isPrefixOp (varName v) = True
  | True = False
hasLowerPrecThanApp (TermApp io (TermImplicitApp False t _) xs) =
  hasLowerPrecThanApp (TermApp io t xs)
hasLowerPrecThanApp t = hasLowerPrecThanPrefixOp t

hasLowerPrecThanPrefixOp :: PreTerm -> Bool
hasLowerPrecThanPrefixOp (TermApp _ (TermVar _ _) [_]) = False
hasLowerPrecThanPrefixOp t@(TermApp _ (TermVar _ v) [_, _])
  | Str.isInfixOp1 (varName v) = True
  | True = hasLowerPrecThanInfixOp1 t
hasLowerPrecThanPrefixOp t@(TermApp _ (TermCtor v _) [_, _])
  | Str.isInfixOp1 (varName v) = True
  | True = hasLowerPrecThanInfixOp1 t
hasLowerPrecThanPrefixOp t@(TermApp _ (TermData v) [_, _])
  | Str.isInfixOp1 (varName v) = True
  | True = hasLowerPrecThanInfixOp1 t
hasLowerPrecThanPrefixOp t@(TermApp _ (TermRef v _) [_, _])
  | Str.isInfixOp1 (varName v) = True
  | True = hasLowerPrecThanInfixOp1 t
hasLowerPrecThanPrefixOp (TermApp io (TermImplicitApp False t _) xs) =
  hasLowerPrecThanPrefixOp (TermApp io t xs)
hasLowerPrecThanPrefixOp t = hasLowerPrecThanInfixOp1 t

hasLowerPrecThanInfixOp1 :: PreTerm -> Bool
hasLowerPrecThanInfixOp1 t@(TermApp _ (TermVar _ v) [_, _])
  | Str.isInfixOp2 (varName v) = True
  | True = hasLowerPrecThanInfixOp2 t
hasLowerPrecThanInfixOp1 t@(TermApp _ (TermCtor v _) [_, _])
  | Str.isInfixOp2 (varName v) = True
  | True = hasLowerPrecThanInfixOp2 t
hasLowerPrecThanInfixOp1 t@(TermApp _ (TermData v) [_, _])
  | Str.isInfixOp2 (varName v) = True
  | True = hasLowerPrecThanInfixOp2 t
hasLowerPrecThanInfixOp1 t@(TermApp _ (TermRef v _) [_, _])
  | Str.isInfixOp2 (varName v) = True
  | True = hasLowerPrecThanInfixOp2 t
hasLowerPrecThanInfixOp1 (TermApp io (TermImplicitApp False t _) xs) =
  hasLowerPrecThanInfixOp1 (TermApp io t xs)
hasLowerPrecThanInfixOp1 t = hasLowerPrecThanInfixOp2 t

hasLowerPrecThanInfixOp2 :: PreTerm -> Bool
hasLowerPrecThanInfixOp2 t@(TermApp _ (TermVar _ v) [_, _])
  | Str.isInfixOp3 (varName v) = True
  | True = hasLowerPrecThanInfixOp3 t
hasLowerPrecThanInfixOp2 t@(TermApp _ (TermCtor v _) [_, _])
  | Str.isInfixOp3 (varName v) = True
  | True = hasLowerPrecThanInfixOp3 t
hasLowerPrecThanInfixOp2 t@(TermApp _ (TermRef v _) [_, _])
  | Str.isInfixOp3 (varName v) = True
  | True = hasLowerPrecThanInfixOp3 t
hasLowerPrecThanInfixOp2 t@(TermApp _ (TermData v) [_, _])
  | Str.isInfixOp3 (varName v) = True
  | True = hasLowerPrecThanInfixOp3 t
hasLowerPrecThanInfixOp2 (TermApp io (TermImplicitApp False t _) xs) =
  hasLowerPrecThanInfixOp2 (TermApp io t xs)
hasLowerPrecThanInfixOp2 t = hasLowerPrecThanInfixOp3 t

hasLowerPrecThanInfixOp3 :: PreTerm -> Bool
hasLowerPrecThanInfixOp3 t@(TermApp _ (TermVar _ v) [_, _])
  | Str.isInfixOp4 (varName v) = True
  | True = hasLowerPrecThanInfixOp4 t
hasLowerPrecThanInfixOp3 t@(TermApp _ (TermCtor v _) [_, _])
  | Str.isInfixOp4 (varName v) = True
  | True = hasLowerPrecThanInfixOp4 t
hasLowerPrecThanInfixOp3 t@(TermApp _ (TermData v) [_, _])
  | Str.isInfixOp4 (varName v) = True
  | True = hasLowerPrecThanInfixOp4 t
hasLowerPrecThanInfixOp3 t@(TermApp _ (TermRef v _) [_, _])
  | Str.isInfixOp4 (varName v) = True
  | True = hasLowerPrecThanInfixOp4 t
hasLowerPrecThanInfixOp3 (TermApp io (TermImplicitApp False t _) xs) =
  hasLowerPrecThanInfixOp3 (TermApp io t xs)
hasLowerPrecThanInfixOp3 t = hasLowerPrecThanInfixOp4 t

hasLowerPrecThanInfixOp4 :: PreTerm -> Bool
hasLowerPrecThanInfixOp4 t@(TermApp _ (TermVar _ v) [_, _])
  | Str.isInfixOp5 (varName v) = True
  | True = hasLowerPrecThanInfixOp5 t
hasLowerPrecThanInfixOp4 t@(TermApp _ (TermCtor v _) [_, _])
  | Str.isInfixOp5 (varName v) = True
  | True = hasLowerPrecThanInfixOp5 t
hasLowerPrecThanInfixOp4 t@(TermApp _ (TermData v) [_, _])
  | Str.isInfixOp5 (varName v) = True
  | True = hasLowerPrecThanInfixOp5 t
hasLowerPrecThanInfixOp4 t@(TermApp _ (TermRef v _) [_, _])
  | Str.isInfixOp5 (varName v) = True
  | True = hasLowerPrecThanInfixOp5 t
hasLowerPrecThanInfixOp4 (TermApp io (TermImplicitApp False t _) xs) =
  hasLowerPrecThanInfixOp4 (TermApp io t xs)
hasLowerPrecThanInfixOp4 t = hasLowerPrecThanInfixOp5 t

hasLowerPrecThanInfixOp5 :: PreTerm -> Bool
hasLowerPrecThanInfixOp5 t@(TermApp _ (TermVar _ v) [_, _])
  | Str.isInfixOp6 (varName v) = True
  | True = hasLowerPrecThanInfixOp6 t
hasLowerPrecThanInfixOp5 t@(TermApp _ (TermCtor v _) [_, _])
  | Str.isInfixOp6 (varName v) = True
  | True = hasLowerPrecThanInfixOp6 t
hasLowerPrecThanInfixOp5 t@(TermApp _ (TermData v) [_, _])
  | Str.isInfixOp6 (varName v) = True
  | True = hasLowerPrecThanInfixOp6 t
hasLowerPrecThanInfixOp5 t@(TermApp _ (TermRef v _) [_, _])
  | Str.isInfixOp6 (varName v) = True
  | True = hasLowerPrecThanInfixOp6 t
hasLowerPrecThanInfixOp5 (TermApp io (TermImplicitApp False t _) xs) =
  hasLowerPrecThanInfixOp5 (TermApp io t xs)
hasLowerPrecThanInfixOp5 t = hasLowerPrecThanInfixOp6 t

hasLowerPrecThanInfixOp6 :: PreTerm -> Bool
hasLowerPrecThanInfixOp6 (TermFun _ _ _ _) = True
hasLowerPrecThanInfixOp6 (TermImplicitApp False (TermFun _ _ _ _) _) = True
hasLowerPrecThanInfixOp6 (TermLazyFun _ _) = True
hasLowerPrecThanInfixOp6 (TermLazyArrow _ _) = True
hasLowerPrecThanInfixOp6 (TermArrow _ _ _) = True
hasLowerPrecThanInfixOp6 _ = False
