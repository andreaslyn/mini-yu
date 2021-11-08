{-# LANGUAGE BangPatterns #-}

module TypeCheck.SubstMap
  ( substPreTerm
  , substCaseTree
  , caseTreeVars
  , preTermVars
  )
where

import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.IntMap as IntMap
import TypeCheck.Term
import TypeCheck.HackGlobalVarId

substPreTerm :: RefMap -> SubstMap -> PreTerm -> PreTerm
substPreTerm = substPreTermNonVisited IntSet.empty

substPreTermNonVisited :: IntSet -> RefMap -> SubstMap -> PreTerm -> PreTerm
substPreTermNonVisited vi rm m t
  | IntMap.null m = t
  | True = doSubstPreTerm vi rm m t

substCaseTree :: RefMap -> SubstMap -> CaseTree -> CaseTree
substCaseTree rm m ct
  | IntMap.null m = ct
  | True = doSubstCaseTree IntSet.empty rm m ct

newSubstVariables :: [Var] -> SubstMap -> ([Var], SubstMap)
newSubstVariables [] su = ([], su)
newSubstVariables (v:vs) su = do
  let !(i, su0) = hackFreshGlobalVarId su
  let v' = mkVar i (varName v)
  let (vs', su') = newSubstVariables vs su0
  (v' : vs', IntMap.insert (varId v) (TermVar False v') su')

substCaseTreeInstance ::
  IntSet -> RefMap -> SubstMap -> ([Var], CaseTree) -> ([Var], CaseTree)
substCaseTreeInstance vi rm subst (is, ct) =
  let (is', subst') = newSubstVariables is subst
      vi' = IntSet.difference vi (IntSet.fromList (map varId is))
  in (is', doSubstCaseTree vi' rm subst' ct)

doSubstCaseTree :: IntSet -> RefMap -> SubstMap -> CaseTree -> CaseTree
doSubstCaseTree vi rm subst (CaseLeaf is t ws) =
  let (is', subst') = newSubstVariables is subst
      vi' = IntSet.difference vi (IntSet.fromList (map varId is))
  in CaseLeaf is' (substPreTermNonVisited vi' rm subst' t) ws
doSubstCaseTree vi rm subst (CaseNode idx m d) =
  let m' = IntMap.map (substCaseTreeInstance vi rm subst) m
      d' = fmap (substCaseTreeInstance vi rm subst) d
  in CaseNode idx m' d'
doSubstCaseTree vi rm subst (CaseUnit idx d) =
  let d' = substCaseTreeInstance vi rm subst d
  in CaseUnit idx d'
doSubstCaseTree _ _ _ (CaseEmpty i) = CaseEmpty i

doSubstPreTerm :: IntSet -> RefMap -> SubstMap -> PreTerm -> PreTerm
doSubstPreTerm vi rm m (TermFun ias io n ct) =
  TermFun ias io n (doSubstCaseTree vi rm m ct)
doSubstPreTerm vi rm m (TermLazyFun io t) =
  TermLazyFun io (doSubstPreTerm vi rm m t)
doSubstPreTerm vi rm m (TermArrow io d c) =
  let d' = map substDom d
      c' = doSubstPreTerm vi rm m' c
  in TermArrow io d' c'
  where
    addMaybeArg :: IntSet -> (Maybe Var, PreTerm) -> IntSet
    addMaybeArg s (Nothing, _) = s
    addMaybeArg s (Just v, _) = IntSet.insert (varId v) s

    m' :: SubstMap
    m' = IntMap.withoutKeys m (foldl addMaybeArg IntSet.empty d)

    substDom :: (Maybe Var, PreTerm) -> (Maybe Var, PreTerm)
    substDom (x, t) = (x, doSubstPreTerm vi rm m' t)
doSubstPreTerm vi rm m (TermLazyArrow io t) =
  TermLazyArrow io (doSubstPreTerm vi rm m t)
doSubstPreTerm vi rm m (TermApp io f xs) =
  let f' = doSubstPreTerm vi rm m f
      xs' = map (doSubstPreTerm vi rm m) xs
  in TermApp io f' xs'
doSubstPreTerm vi rm m (TermImplicitApp b f xs) =
  let f' = doSubstPreTerm vi rm m f
      xs' = fmap (\(n, x) -> (n, doSubstPreTerm vi rm m x)) xs
  in TermImplicitApp b f' xs'
doSubstPreTerm vi rm m (TermLazyApp io f) =
  let f' = doSubstPreTerm vi rm m f
  in TermLazyApp io f'
doSubstPreTerm vi rm m (TermCase t ct) =
  let t' = doSubstPreTerm vi rm m t
      ct' = doSubstCaseTree vi rm m ct
  in TermCase t' ct'
doSubstPreTerm vi rm m (TermRef v s) =
  -- First substitute with s and then substitute with m.
  -- So the terms inside s need to be substituted with m:
  let s' = IntMap.map (doSubstPreTerm vi rm m) s
      m' = IntMap.restrictKeys m (preTermVars rm (TermRef v s))
  in TermRef v (IntMap.union s' m') -- prefers s'.
doSubstPreTerm _ _ _ (TermData v) = TermData v
doSubstPreTerm _ _ _ (TermCtor v i) = TermCtor v i
doSubstPreTerm _ _ _ TermUnitElem = TermUnitElem
doSubstPreTerm _ _ _ TermUnitTy = TermUnitTy
doSubstPreTerm _ _ _ TermEmpty = TermEmpty
doSubstPreTerm _ _ _ TermTy = TermTy
doSubstPreTerm vi rm m (TermVar b v) =
  if IntSet.member (varId v) vi
  then TermVar b v
  else
    case IntMap.lookup (varId v) m of
      Nothing -> TermVar b v
      Just t -> doSubstPreTerm (IntSet.insert (varId v) vi) rm m t

caseTreeVars :: IntSet -> RefMap -> CaseTree -> VarIdSet
caseTreeVars vis rm = caseVars
  where
    caseVars :: CaseTree -> VarIdSet
    caseVars (CaseLeaf is t _) =
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
  let (bs, as') = foldl f (IntSet.empty, IntSet.empty) as
      a' = doPreTermVars vis r t
  in IntSet.difference (IntSet.union a' as') bs
  where
    f :: (VarIdSet, VarIdSet) -> (Maybe Var, PreTerm) -> (VarIdSet, VarIdSet)
    f (bs, fs) (Nothing, s) =
      (bs, IntSet.union fs (doPreTermVars vis r s))
    f (bs, fs) (Just v, s) =
      (IntSet.insert (varId v) bs, IntSet.union fs (doPreTermVars vis r s))
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
              Just (t, me) ->
                if refMetaIsGlobal me
                then IntSet.empty
                else doPreTermVars vis' r (substPreTerm r su (termPre t))
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
