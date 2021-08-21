{-# LANGUAGE BangPatterns #-}

module TypeCheck.SubstMap
  ( substPreTerm
  , substCaseTree
  )
where

import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.IntMap as IntMap
import TypeCheck.Term
import TypeCheck.HackGlobalVarId

substPreTerm :: SubstMap -> PreTerm -> PreTerm
substPreTerm = substPreTermNonVisited IntSet.empty

substPreTermNonVisited :: IntSet -> SubstMap -> PreTerm -> PreTerm
substPreTermNonVisited vi m t
  | IntMap.null m = t
  | True = doSubstPreTerm vi m t

substCaseTree :: SubstMap -> CaseTree -> CaseTree
substCaseTree m ct
  | IntMap.null m = ct
  | True = doSubstCaseTree IntSet.empty m ct

newSubstVariables :: [Var] -> SubstMap -> ([Var], SubstMap)
newSubstVariables [] su = ([], su)
newSubstVariables (v:vs) su = do
  let !(i, su0) = hackFreshGlobalVarId su
  let v' = mkVar i (varName v)
  let (vs', su') = newSubstVariables vs su0
  (v' : vs', IntMap.insert (varId v) (TermVar False v') su')

substCaseTreeInstance ::
  IntSet -> SubstMap -> ([Var], CaseTree) -> ([Var], CaseTree)
substCaseTreeInstance vi subst (is, ct) =
  let (is', subst') = newSubstVariables is subst
      vi' = IntSet.difference vi (IntSet.fromList (map varId is))
  in (is', doSubstCaseTree vi' subst' ct)

doSubstCaseTree :: IntSet -> SubstMap -> CaseTree -> CaseTree
doSubstCaseTree vi subst (CaseLeaf is io t ws) =
  let (is', subst') = newSubstVariables is subst
      vi' = IntSet.difference vi (IntSet.fromList (map varId is))
  in CaseLeaf is' io (substPreTermNonVisited vi' subst' t) ws
doSubstCaseTree vi subst (CaseNode idx m d) =
  let m' = IntMap.map (substCaseTreeInstance vi subst) m
      d' = fmap (substCaseTreeInstance vi subst) d
  in CaseNode idx m' d'
doSubstCaseTree vi subst (CaseUnit idx d) =
  let d' = substCaseTreeInstance vi subst d
  in CaseUnit idx d'
doSubstCaseTree _ _ (CaseEmpty i) = CaseEmpty i

doSubstPreTerm :: IntSet -> SubstMap -> PreTerm -> PreTerm
doSubstPreTerm vi m (TermFun ias io n ct) =
  TermFun ias io n (doSubstCaseTree vi m ct)
doSubstPreTerm vi m (TermLazyFun io t) =
  TermLazyFun io (doSubstPreTerm vi m t)
doSubstPreTerm vi m (TermArrow io d c) =
  let d' = map substDom d
      c' = doSubstPreTerm vi m' c
  in TermArrow io d' c'
  where
    addMaybeArg :: IntSet -> (Maybe Var, PreTerm) -> IntSet
    addMaybeArg s (Nothing, _) = s
    addMaybeArg s (Just v, _) = IntSet.insert (varId v) s

    m' :: SubstMap
    m' = IntMap.withoutKeys m (foldl addMaybeArg IntSet.empty d)

    substDom :: (Maybe Var, PreTerm) -> (Maybe Var, PreTerm)
    substDom (x, t) = (x, doSubstPreTerm vi m' t)
doSubstPreTerm vi m (TermLazyArrow io t) =
  TermLazyArrow io (doSubstPreTerm vi m t)
doSubstPreTerm vi m (TermApp io f xs) =
  let f' = doSubstPreTerm vi m f
      xs' = map (doSubstPreTerm vi m) xs
  in TermApp io f' xs'
doSubstPreTerm vi m (TermImplicitApp b f xs) =
  let f' = doSubstPreTerm vi m f
      xs' = fmap (\(n, x) -> (n, doSubstPreTerm vi m x)) xs
  in TermImplicitApp b f' xs'
doSubstPreTerm vi m (TermLazyApp io f) =
  let f' = doSubstPreTerm vi m f
  in TermLazyApp io f'
doSubstPreTerm vi m (TermMatch t ct) =
  let t' = doSubstPreTerm vi m t
      ct' = doSubstCaseTree vi m ct
  in TermMatch t' ct'
doSubstPreTerm vi m (TermRef v s) =
  -- First substitute with s and then substitute with m.
  -- So the terms inside s need to be substituted with m:
  let s' = IntMap.map (doSubstPreTerm vi m) s
  in TermRef v (IntMap.union s' m) -- prefers s'.
doSubstPreTerm _ _ (TermData v) = TermData v
doSubstPreTerm _ _ (TermCtor v i) = TermCtor v i
doSubstPreTerm _ _ TermUnitElem = TermUnitElem
doSubstPreTerm _ _ TermUnitTy = TermUnitTy
doSubstPreTerm _ _ TermEmpty = TermEmpty
doSubstPreTerm _ _ TermTy = TermTy
doSubstPreTerm vi m (TermVar b v) =
  if IntSet.member (varId v) vi
  then TermVar b v
  else
    case IntMap.lookup (varId v) m of
      Nothing -> TermVar b v
      Just t -> doSubstPreTerm (IntSet.insert (varId v) vi) m t
