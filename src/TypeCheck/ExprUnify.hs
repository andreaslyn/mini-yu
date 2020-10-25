{-# LANGUAGE BangPatterns #-}

module TypeCheck.ExprUnify
  ( ExprUnifResult
  , tcExprSubstUnify
  , runExprUnifResult
  , mergeExprUnifMaps
  )
where

import Str (quote)
import Loc
import TypeCheck.Env as Env
import TypeCheck.TypeCheckT
import TypeCheck.Term
import TypeCheck.TermEnv
import TypeCheck.SubstMap
import TypeCheck.UnifyCommon

import Data.Foldable (foldlM)
import Control.Monad.Except
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromJust)
import Control.Exception (assert)



import Data.Maybe (isJust, isNothing)
import Data.List (sortOn)
--import Debug.Trace (trace)


type ExprUnifResult m = ExceptT String (TypeCheckT m) SubstMap

tcExprSubstUnify :: Monad m => Loc -> PreTerm -> PreTerm -> ExprT m ()
tcExprSubstUnify lo expectedTy actualTy = do
  su0 <- getExprSubst
  tcExprUnify lo (substPreTerm su0 expectedTy) (substPreTerm su0 actualTy)
  return ()
























preTermsAlphaEqualIM :: ImplicitMap -> RefMap -> PreTerm -> PreTerm -> Bool
preTermsAlphaEqualIM = doPreTermsAlphaEqualIM []

type RefBisim = [((Var, SubstMap), (Var, SubstMap))]

-- The case for TermRef is dependent on the argument order is kept
-- in recursive calls.
doPreTermsAlphaEqualIM :: RefBisim -> ImplicitMap -> RefMap -> PreTerm -> PreTerm -> Bool
doPreTermsAlphaEqualIM _ _ _ (TermVar _ v1) (TermVar _ v2) = varId v1 == varId v2
doPreTermsAlphaEqualIM _ _ _ (TermData v1) (TermData v2) = varId v1 == varId v2
doPreTermsAlphaEqualIM _ _ _ (TermCtor v1 _) (TermCtor v2 _) = varId v1 == varId v2
doPreTermsAlphaEqualIM _ _ _ TermUnitElem TermUnitElem = True
doPreTermsAlphaEqualIM _ _ _ TermUnitTy TermUnitTy = True
doPreTermsAlphaEqualIM _ _ _ TermEmpty TermEmpty = True
doPreTermsAlphaEqualIM _ _ _ TermTy TermTy = True
doPreTermsAlphaEqualIM bi im r (TermArrow io1 as1 a1) (TermArrow io2 as2 a2) =
  io1 == io2
  && length as1 == length as2
  && let as = zip as1 as2
         su = foldl addArrowSubst IntMap.empty as
         as2' = map (substArrow su) as2
     in (and (map (uncurry (doPreTermsAlphaEqualIM bi im r)) (zip (map snd as1) (map snd as2')))
        && doPreTermsAlphaEqualIM bi im r a1 (substPreTerm su a2))
          || error "term arrow"
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
doPreTermsAlphaEqualIM bi im r (TermLazyArrow io1 a1) (TermLazyArrow io2 a2) =
  (io1 == io2 && doPreTermsAlphaEqualIM bi im r a1 a2)
      || error "term lazy arrow"
doPreTermsAlphaEqualIM bi im r (TermApp io1 f xs) (TermApp io2 g ys) =
  ((io1 == io2 || error "term app io")
  && (length xs == length ys || error "term app length")
  && (doPreTermsAlphaEqualIM bi im r f g || error "term app base")
  && (and (map (uncurry (doPreTermsAlphaEqualIM bi im r)) (zip xs ys))) || error ("app end: " ++ preTermToString im r defaultExprIndent (TermApp io1 f xs) ++ "\n" ++ " and\n" ++ preTermToString im r defaultExprIndent (TermApp io2 g ys)))
doPreTermsAlphaEqualIM bi im r (TermImplicitApp _ f xs) (TermImplicitApp _ g ys) =
  let (xs0, xs1) = unzip (sortOn fst xs)
      (ys0, ys1) = unzip (sortOn fst ys)
  in (xs0 == ys0
     && doPreTermsAlphaEqualIM bi im r f g
     && and (map (uncurry (doPreTermsAlphaEqualIM bi im r)) (zip xs1 ys1)))
      || error "term implicit app"
doPreTermsAlphaEqualIM bi im r (TermLazyApp io1 f) (TermLazyApp io2 g) =
  (io1 == io2 && doPreTermsAlphaEqualIM bi im r f g)
  || error "term lazy app"
doPreTermsAlphaEqualIM bi im r (TermLazyFun io1 t1) (TermLazyFun io2 t2) =
  (io1 == io2 && doPreTermsAlphaEqualIM bi im r t1 t2)
  || error "term lazy fun"
doPreTermsAlphaEqualIM bi im r (TermFun ias1 io1 _ ct1) (TermFun ias2 io2 _ ct2) =
  -- Number of explicit arguments are checked by case tree comparison.
  -- The order of the implicit arguments matters, since the case trees
  -- depend on the order.
  (io1 == io2 && ias1 == ias2 && caseTreesAlphaEqualIM bi IntMap.empty im r ct1 ct2)
  || error "term fun"
doPreTermsAlphaEqualIM bi im r (TermRef v1 s1') (TermRef v2 s2')
  | isNothing (IntMap.lookup (varId v1) r)
    && isNothing (IntMap.lookup (varId v2) r) = varId v1 == varId v2 || error "ref ref id"
  | isNothing (IntMap.lookup (varId v1) r)
    && isJust (IntMap.lookup (varId v2) r) = False || error "non-ref ref"
  | isJust (IntMap.lookup (varId v1) r)
    && isNothing (IntMap.lookup (varId v2) r) = False || error "ref non-ref"
  | True =
  let s1 = IntMap.restrictKeys s1' (preTermVars r (TermRef v1 IntMap.empty))
      s2 = IntMap.restrictKeys s2' (preTermVars r (TermRef v2 IntMap.empty))
      p1 = (v1, s1)
      p2 = (v2, s2)
  in (termRefsTest p1 p2
     || bisimHasPair p1 p2 bi
     || let r1 = Env.forceLookupRefMap (varId v1) r
            r2 = Env.forceLookupRefMap (varId v2) r
            r1' = substPreTerm s1 (termPre r1)
            r2' = substPreTerm s2 (termPre r2)
        in doPreTermsAlphaEqualIM ((p1, p2) : bi) im r r1' r2')
            || error "ref ref"
  where
    termRefsTest :: (Var, SubstMap) -> (Var, SubstMap) -> Bool
    termRefsTest (u1, r1) (u2, r2) = do
      varId u1 == varId u2
      && IntMap.keysSet r1 == IntMap.keysSet r2
         && let g1 = IntMap.toList r1
                g2 = IntMap.toList r2
                z = zip g1 g2
            in and (map (\(x1, x2) ->
                     doPreTermsAlphaEqualIM bi im r (snd x1) (snd x2)) z)

    bisimHasPair :: (Var, SubstMap) -> (Var, SubstMap) -> RefBisim -> Bool
    bisimHasPair _ _ [] = False
    bisimHasPair x1 x2 ((y1, y2) : bi') =
      (termRefsTest x1 y1 && termRefsTest x2 y2) || bisimHasPair x1 x2 bi'
doPreTermsAlphaEqualIM bi im r (TermRef v1 s1) t2
  | isNothing (IntMap.lookup (varId v1) r) = False || error "ref left 1"
  | True =
  let r1 = Env.forceLookupRefMap (varId v1) r
      r1' = substPreTerm s1 (termPre r1)
  in doPreTermsAlphaEqualIM bi im r r1' t2 || error "ref left 2"
doPreTermsAlphaEqualIM bi im r t1 (TermRef v2 s2)
  | isNothing (IntMap.lookup (varId v2) r) = False || error "ref right 1"
  | True =
  let r2 = Env.forceLookupRefMap (varId v2) r
      r2' = substPreTerm s2 (termPre r2)
  in doPreTermsAlphaEqualIM bi im r t1 r2'
      || error "ref right 2"
doPreTermsAlphaEqualIM bi im r (TermLazyFun io t1) t2 =
  doPreTermsAlphaEqualIM bi im r t1 (TermLazyApp io t2)
  || error "lazy fun left"
doPreTermsAlphaEqualIM bi im r t1 (TermLazyFun io t2) =
  doPreTermsAlphaEqualIM bi im r (TermLazyApp io t1) t2
  || error "lazy fun right"
doPreTermsAlphaEqualIM bi im r (TermFun [] io1 _ (CaseLeaf i1 _ t1 _)) t2 =
 (doPreTermsAlphaEqualIM bi im r
    t1 (TermApp io1 t2 (map (\i -> TermVar False (mkVar (varId i) "_")) i1)))
 || error "term fun case tree left"
doPreTermsAlphaEqualIM bi im r t1 (TermFun [] io2 _ (CaseLeaf i2 _ t2 _)) =
 (doPreTermsAlphaEqualIM bi im r
    (TermApp io2 t1 (map (\i -> TermVar False (mkVar (varId i) "_")) i2)) t2)
 || error "term fun case tree right"
doPreTermsAlphaEqualIM bi im r (TermCase t1 ct1) (TermCase t2 ct2) =
 (doPreTermsAlphaEqualIM bi im r t1 t2
  && caseTreesAlphaEqualIM bi IntMap.empty im r ct1 ct2)
 || error "term case tree"
doPreTermsAlphaEqualIM _ _ _ _ _ = error "PRE TERM DEFAULT"

caseTreesAlphaEqualIM ::
  RefBisim -> SubstMap -> ImplicitMap -> RefMap -> CaseTree -> CaseTree -> Bool
caseTreesAlphaEqualIM bi subst im rm (CaseLeaf i1 io1 t1 _) (CaseLeaf i2 io2 t2 _) = do
 ((io1 == io2 || error "LEAF IO")
  && (length i1 == length i2 || error "LEAF LENGTH")
  && let su0 = if map varId i1 /= map varId i2
                then IntMap.fromList (zip (map varId i2) (map (TermVar False) i1))
                else IntMap.empty
         su = IntMap.union subst su0
     in doPreTermsAlphaEqualIM bi im rm t1 (substPreTerm su t2))
         || error "LEAF PRE TERMS"
caseTreesAlphaEqualIM bi subst im rm (CaseNode idx1 m1 d1) (CaseNode idx2 m2 d2) =
 (idx1 == idx2
  && IntMap.keysSet m1 == IntMap.keysSet m2
  && isJust d1 == isJust d2
  && let g1 = IntMap.toList m1
         g2 = IntMap.toList m2
         z = zip g1 g2
      in and (map (\(x1, x2) ->
               caseTreeInstanceAlphaEqualIM bi subst im rm (snd x1) (snd x2)) z)
         && case (d1, d2) of
              (Nothing, Nothing) -> True
              (Just d1', Just d2') ->
                caseTreeInstanceAlphaEqualIM bi subst im rm d1' d2'
              _ -> error "unexpected case")
                        ||  error "CASE TREE NODE"
caseTreesAlphaEqualIM bi subst im rm (CaseUnit idx1 d1) (CaseUnit idx2 d2) =
  (idx1 == idx2 && caseTreeInstanceAlphaEqualIM bi subst im rm d1 d2)
  ||  error "CASE TREE UNIT"
caseTreesAlphaEqualIM _ _ _ _ (CaseEmpty idx1) (CaseEmpty idx2) =
  idx1 == idx2 || error "CASE TREE EMPTY"
caseTreesAlphaEqualIM _ _ _ _ _ _ = error "CASE TREE"

caseTreeInstanceAlphaEqualIM ::
  RefBisim -> SubstMap -> ImplicitMap -> RefMap -> ([Var], CaseTree) -> ([Var], CaseTree) -> Bool
caseTreeInstanceAlphaEqualIM bi subst im rm (i1, c1) (i2, c2) =
  length i1 == length i2
  && let su0 = if map varId i1 /= map varId i2
                then IntMap.fromList (zip (map varId i2) (map (TermVar False) i1))
                else IntMap.empty
         su = IntMap.union subst su0
     in caseTreesAlphaEqualIM bi su im rm c1 c2










































tcExprUnify :: Monad m =>
  Loc -> PreTerm -> PreTerm -> ExprT m ()
tcExprUnify lo t1 t2 = do
  runExprUnifResult lo True msgPrefix t1 t2
  where
    msgPrefix :: Monad m => ExprT m String
    msgPrefix = do
      isu <- getExprSubst
      let t1' = substPreTerm isu t1
      let t2' = substPreTerm isu t2
      im <- lift Env.getImplicitMap
      rm <- lift Env.getRefMap
      return $
        "expected expression to have type\n"
        ++ preTermToString im rm defaultExprIndent (preTermNormalize rm t1')
        ++ "\nbut type is\n"
        ++ preTermToString im rm defaultExprIndent (preTermNormalize rm t2')
        ++ "\n" ++ show (preTermsAlphaEqualIM im rm (preTermNormalize rm t1') (preTermNormalize rm t2'))

runExprUnifResult :: Monad m =>
  Loc -> Bool -> ExprT m String -> PreTerm -> PreTerm -> ExprT m ()
runExprUnifResult lo normalize msgPrefix t1 t2 = do
  boundIds <- lift Env.getNextVarId
  r <- lift (runExceptT (exprUnifWithBoundIds normalize boundIds t1 t2))
  case r of
    Right m -> do
      isu <- getExprSubst
      isu' <- lift (runExceptT (mergeExprUnifMaps boundIds m isu))
      isu'' <- case isu' of
                Right x -> return x
                Left msg -> errMsg msg
      putExprSubst isu''
    Left msg -> errMsg msg
    where
      errMsg m = do
        p <- msgPrefix
        let m' = if m /= "" then p ++ "\n" ++ m else p
        lift (err lo (Recoverable m'))

exprUnifWithBoundIds :: Monad m =>
  Bool -> VarId -> PreTerm -> PreTerm -> ExprUnifResult m
exprUnifWithBoundIds normalize boundIds = \t1 t2 -> do
  r <- lift Env.getRefMap
  --i0 <- lift Env.getImplicitMap
  --let !_ = trace ("expr unify " ++ preTermToString i0 r (preTermNormalize r t1) ++ " with " ++ preTermToString i0 r (preTermNormalize r t2)) ()
  if normalize
  then eunify2 (preTermNormalize r t1) (preTermNormalize r t2)
  else eunify2 t1 t2
  where
    eunify :: Monad m => [(PreTerm, PreTerm)] -> SubstMap -> ExprUnifResult m
    eunify termPairs = \su0 -> do
      (msg, rest, subst) <- doEunify termPairs su0
      if null rest
      then return subst
      else
        if length rest == length termPairs
        then throwError msg
        else do
          rest' <- lift (substPairs rest subst)
          eunify rest' subst
      where
        doEunify :: Monad m =>
          [(PreTerm, PreTerm)] -> SubstMap ->
          ExceptT String (TypeCheckT m) (String, [(PreTerm, PreTerm)], SubstMap)
        doEunify [] su0 = return ("", [], su0)
        doEunify ((t1, t2) : ts) su0 =
          catchError
            (do
              m1 <- eunify2 t1 t2
              su1 <- mergeExprUnifMaps boundIds su0 m1
              ts' <- lift (substPairs ts su1)
              (er, rs, m2) <- doEunify ts' su1
              su2 <- mergeExprUnifMaps boundIds su1 m2
              return (er, rs, su2))
            (\msg -> do
              (_, rs, su) <- doEunify ts su0
              return (msg, (t1, t2) : rs, su))

    substPairs :: Monad m =>
      [(PreTerm, PreTerm)] -> SubstMap -> TypeCheckT m [(PreTerm, PreTerm)]
    substPairs ps m = do
      r <- Env.getRefMap
      if normalize
      then
        return $
          map (\(x,y) -> (preTermNormalize r (substPreTerm m x),
                          preTermNormalize r (substPreTerm m y))) ps
      else
        return $
          map (\(x,y) -> (substPreTerm m x, substPreTerm m y)) ps

    eunify2 :: Monad m => PreTerm -> PreTerm -> ExprUnifResult m
    eunify2 t1 t2 =
      case (varBaseTerm t1, varBaseTerm t2) of
        (Just _, _) -> unifyVarApp t1 t2
        (_, Just _) -> unifyVarApp t1 t2
        _ -> eunify2NotVar t1 t2

    eunify2NotVar :: Monad m => PreTerm -> PreTerm -> ExprUnifResult m
    eunify2NotVar ar1@(TermArrow io1 d1 c1) ar2@(TermArrow io2 d2 c2) = do
      r0 <- lift Env.getRefMap
      im <- lift Env.getImplicitMap
      if io2 && not io1
      then
        throwError $
            "unable to assign effectful function type\n"
            ++ preTermToString im r0 defaultExprIndent (TermArrow io2 d2 c2) ++ "\n"
            ++ "to regular function type\n"
            ++ preTermToString im r0 defaultExprIndent (TermArrow io1 d1 c1)
      else if length d1 /= length d2
        then
          throwError "function types with different arities"
        else
          catchError (
            do
              -- Flip arguments for unification of coercions like
              -- regular arrow => effectful arrow.
              let ds = zip d2 d1
              su <- foldlM updateArrowMap IntMap.empty ds
              let ds' = map (\(x,y) -> (substPreTerm su (snd x),
                                        substPreTerm su (snd y))) ds
              r <- lift Env.getRefMap
              m <- eunify ds' IntMap.empty
              u <- if normalize
                    then
                      eunify2
                        (preTermNormalize r (substPreTerm m (substPreTerm su c1)))
                        (preTermNormalize r (substPreTerm m (substPreTerm su c2)))
                    else
                      eunify2
                        (substPreTerm m (substPreTerm su c1))
                        (substPreTerm m (substPreTerm su c2))
              mergeExprUnifMaps boundIds m u)
            (\_ -> unifyAlpha ar1 ar2)
      where
        updateArrowMap :: Monad m =>
          SubstMap -> ((Maybe Var, PreTerm), (Maybe Var, PreTerm)) ->
          ExprUnifResult m
        updateArrowMap _ ((Nothing, _), (Nothing, _)) = return IntMap.empty
        updateArrowMap m ((Just n, _), (Nothing, _)) = do
          i <- lift Env.freshVarId
          return (IntMap.insert (varId n)
                    (TermVar False (mkVar i (varName n))) m)
        updateArrowMap m ((Nothing, x), (Just n, y)) =
          updateArrowMap m ((Just n, y), (Nothing, x))
        updateArrowMap m ((Just n1, _), (Just n2, _)) = do
          i <- lift Env.freshVarId
          let v = TermVar False (mkVar i (varName n1))
          let x = IntMap.insert (varId n1) v m
          return (IntMap.insert (varId n2) v x)
    eunify2NotVar (TermLazyArrow io1 c1) (TermLazyArrow io2 c2) = do
      r0 <- lift Env.getRefMap
      im <- lift Env.getImplicitMap
      if io2 && not io1
      then
        throwError $
            "unable to assign effectful lazy type\n"
            ++ preTermToString im r0 defaultExprIndent (TermLazyArrow io2 c2) ++ "\n"
            ++ "to regular lazy type\n"
            ++ preTermToString im r0 defaultExprIndent (TermLazyArrow io1 c1)
      else eunify2 c1 c2
    eunify2NotVar t1@(TermApp _ f1 x1) t2@(TermApp _ f2 x2) =
      if canAppUnify f1 && canAppUnify f2
      then do
        r <- lift Env.getRefMap
        im <- lift Env.getImplicitMap
        s1 <- eunify2 f1 f2
        if length x1 /= length x2
          then throwError $
                  "unable to unify\n"
                  ++ preTermToString im r defaultExprIndent t1 ++ "\n"
                  ++ "with\n"
                  ++ preTermToString im r defaultExprIndent t2 ++ "\n"
                  ++ "different arities"
          else eunify (zip x1 x2) s1
      else unifyAlpha t1 t2
    eunify2NotVar t1@(TermImplicitApp _ f1 x1) t2@(TermImplicitApp _ f2 x2) =
      if canAppUnify f1 && canAppUnify f2
      then do
        s1 <- eunify2 f1 f2
        r <- lift Env.getRefMap
        im <- lift Env.getImplicitMap
        if length x1 /= length x2
          then throwError $
                  "unable to unify\n"
                  ++ preTermToString im r defaultExprIndent t1 ++ "\n"
                  ++ "with\n"
                  ++ preTermToString im r defaultExprIndent t2 ++ "\n"
                  ++ "different implicit arities"
          else do
            let z = map (\(a, b) -> (snd a, snd b)) (zip x1 x2)
            eunify z s1
      else unifyAlpha t1 t2
    eunify2NotVar t1@(TermLazyApp _ f1) t2@(TermLazyApp _ f2) =
      if canAppUnify f1 && canAppUnify f2
        then eunify2 f1 f2
        else unifyAlpha t1 t2
    eunify2NotVar (TermCtor v1 _) (TermCtor v2 _) =
      if varId v1 == varId v2
        then return IntMap.empty
        else throwError $
                "cannot unify distinct constructors "
                ++ quote (varName v1) ++ " and " ++ quote (varName v2)
    eunify2NotVar (TermData v1) (TermData v2) =
      if varId v1 == varId v2
        then return IntMap.empty
        else throwError $
                "cannot unify distinct data types "
                ++ quote (varName v1) ++ " and " ++ quote (varName v2)
    eunify2NotVar TermTy TermTy = return IntMap.empty
    eunify2NotVar TermUnitElem TermUnitElem = return IntMap.empty
    eunify2NotVar TermUnitTy TermUnitTy = return IntMap.empty
    eunify2NotVar f1@(TermLazyFun io1 t1) f2@(TermLazyFun io2 t2) = do
      if io1 /= io2
      then throwError $
            "unable to unify effectful lazy with pure lazy"
      else
        if io1
        then
          unifyAlpha f1 f2
        else
          catchError (eunify2 t1 t2) (\_ -> unifyAlpha f1 f2)
    eunify2NotVar f1@(TermFun ias1 io1 _ (CaseLeaf i1 _ t1 _))
                  f2@(TermFun ias2 io2 _ (CaseLeaf i2 _ t2 _))
      | io1 == io2 && ias1 == ias2 && length i1 == length i2 =
        if io1
        then
          unifyAlpha f1 f2
        else
          catchError
            (do
              (_, su1) <- lift (makeNewVarIds i1)
              let upd m (j1, j2) = IntMap.insert (varId j2)
                                    (fromJust (IntMap.lookup (varId j1) su1)) m
              let su2 = foldl upd IntMap.empty (zip i1 i2)
              let t1' = substPreTerm su1 t1
              let t2' = substPreTerm su2 t2
              eunify2 t1' t2')
            (\_ -> unifyAlpha f1 f2)
      | io1 /= io2 =
          throwError $
            "unable to unify effectful function with regular function"
      | length i1 /= length i2 =
          throwError $ "unable to unify functions with different arities"
      | True =
          throwError $ "unable to unify values with different implicits"
    eunify2NotVar f1@(TermLazyFun False t1) t2 =
      catchError (eunify2 t1 (TermLazyApp False t2))
        (\_ -> unifyAlpha f1 t2)
    eunify2NotVar t1 f2@(TermLazyFun False t2) =
      catchError (eunify2 (TermLazyApp False t1) t2)
        (\_ -> unifyAlpha t1 f2)
    eunify2NotVar f1@(TermFun [] False _ (CaseLeaf i1 io t1 _)) t2 = do
      let !() = assert (not io) ()
      catchError
        (do
          (vs, su) <- lift (makeNewVarIds i1)
          let t1' = substPreTerm su t1
          eunify2 t1' (TermApp False t2 vs))
        (\_ -> unifyAlpha f1 t2)
    eunify2NotVar t1 f2@(TermFun [] False _ (CaseLeaf i2 io t2 _)) = do
      let !() = assert (not io) ()
      catchError
        (do
          (vs, su) <- lift (makeNewVarIds i2)
          let t2' = substPreTerm su t2
          eunify2 (TermApp False t1 vs) t2')
        (\_ -> unifyAlpha t1 f2)
    eunify2NotVar t1 t2 = unifyAlpha t1 t2

    makeNewVarIds :: Monad m => [Var] -> TypeCheckT m ([PreTerm], SubstMap)
    makeNewVarIds [] = return ([], IntMap.empty)
    makeNewVarIds (i:is) = do
      (vs, su) <- makeNewVarIds is
      i' <- Env.freshVarId
      let v = TermVar False (mkVar i' "_")
      let vs' = v : vs
      let su' = IntMap.insert (varId i) v su
      return (vs', su')

    isBoundVar :: VarId -> Bool
    isBoundVar i = i >= boundIds

    hasBoundVar :: Monad m => PreTerm -> TypeCheckT m Bool
    hasBoundVar t = do
      r <- Env.getRefMap
      return (preTermExistsVar r isBoundVar t)

    allVars :: [PreTerm] -> Maybe [Var]
    allVars [] = Just []
    allVars (TermVar _ v : ts) = fmap (v:) (allVars ts)
    allVars (_:_) = Nothing

    varBaseTerm :: PreTerm -> Maybe Var
    varBaseTerm (TermApp _ f _) = varBaseTerm f
    varBaseTerm (TermLazyApp _ f) = varBaseTerm f
    varBaseTerm (TermVar True v) = Just v
    varBaseTerm (TermVar False _) = Nothing
    varBaseTerm _ = Nothing

    varBaseVars :: PreTerm -> [PreTerm] -> Maybe (Var, [Var])
    varBaseVars p ts = do
      v <- varBaseTerm p
      vs <- allVars ts
      Just (v, vs)

    unifyVarApp :: Monad m => PreTerm -> PreTerm -> ExprUnifResult m
    unifyVarApp t1 t2 =
      catchError (doUnifyVarApp t1 t2)
        (\_ ->
          catchError (unifyVar t1 t2)
            (\msg -> if msg == "" then unifyAlpha t1 t2 else throwError msg))

    doUnifyVarApp :: Monad m => PreTerm -> PreTerm -> ExprUnifResult m
    doUnifyVarApp t1@(TermVar True _) t2 = unifyVar t1 t2
    doUnifyVarApp t1 t2@(TermVar True _) = unifyVar t1 t2
    doUnifyVarApp (TermApp _ f1 []) (TermApp _ f2 []) =
      doUnifyVarApp f1 f2
    doUnifyVarApp t1@(TermApp io1 f1 as1) t2@(TermApp io2 f2 as2) =
      case varBaseVars f1 as1 of
        Just (v1, vs1) ->
          case varBaseVars f2 as2 of
            Just (v2, vs2) -> do
              rm <- lift Env.getRefMap
              if preTermListsEqual rm as1 as2
                 || varId v1 == varId v2 -- To allow empty subst map
                                         -- when the terms are equal.
              then doUnifyVarApp f1 f2
              else
                if varId v1 > varId v2
                then do
                  f2' <- lift (makeFunWithVarSubst io2 vs1 t2)
                  doUnifyVarApp f1 f2'
                else do
                  f1' <- lift (makeFunWithVarSubst io1 vs2 t1)
                  doUnifyVarApp f1' f2
            Nothing -> do
              f2' <- lift (makeFunWithVarSubst io2 vs1 t2)
              doUnifyVarApp f1 f2'
        Nothing ->
          case varBaseVars f2 as2 of
            Just (_, vs2) -> do
              f1' <- lift (makeFunWithVarSubst io1 vs2 t1)
              doUnifyVarApp f1' f2
            Nothing -> throwError ""
    doUnifyVarApp (TermApp io f1 as1) t2 =
      case varBaseVars f1 as1 of
        Nothing -> throwError ""
        Just (_, vs1) -> do
          f2' <- lift (makeFunWithVarSubst io vs1 t2)
          doUnifyVarApp f1 f2'
    doUnifyVarApp t1 (TermApp io f2 as2) =
      case varBaseVars f2 as2 of
        Nothing -> throwError ""
        Just (_, vs2) -> do
          f1' <- lift (makeFunWithVarSubst io vs2 t1)
          doUnifyVarApp f1' f2
    doUnifyVarApp (TermLazyApp _ f1) (TermLazyApp _ f2) =
      doUnifyVarApp f1 f2
    doUnifyVarApp (TermLazyApp io f1) t2 =
      case varBaseTerm f1 of
        Nothing -> throwError ""
        Just _ ->
          doUnifyVarApp f1 (TermLazyFun io t2)
    doUnifyVarApp t1 (TermLazyApp io f2) =
      case varBaseTerm f2 of
        Nothing -> throwError ""
        Just _ ->
          doUnifyVarApp (TermLazyFun io t1) f2
    doUnifyVarApp t1 t2 = unifyVar t1 t2

    unifyVar :: Monad m => PreTerm -> PreTerm -> ExprUnifResult m
    unifyVar (TermVar b1 v1) (TermVar b2 v2)
      | varId v1 == varId v2 = return IntMap.empty
      | isBoundVar (varId v1) && isBoundVar (varId v2) =
          throwError $
            "unable to unify distinct bound variables "
            ++ quote (varName v1) ++ " and " ++ quote (varName v2)
      | isBoundVar (varId v1) =
          throwError $
            "unable to unify variable " ++ quote (varName v2)
            ++ " with bound variable " ++ quote (varName v1)
      | isBoundVar (varId v2) =
          throwError $
            "unable to unify variable " ++ quote (varName v1)
            ++ " with bound variable " ++ quote (varName v2)
      | b1 && b2 && varId v1 < varId v2 =
          return (IntMap.singleton (varId v2) (TermVar b1 v1))
      | b1 && b2 && varId v2 < varId v1 =
          return (IntMap.singleton (varId v1) (TermVar b2 v2))
      | b1 && not b2 =
          return (IntMap.singleton (varId v1) (TermVar b2 v2))
      | not b1 && b2 =
          return (IntMap.singleton (varId v2) (TermVar b1 v1))
      | True =
          throwError $
            "unable to unify distinct variables "
            ++ quote (varName v1) ++ " and " ++ quote (varName v2)
    unifyVar (TermVar True v1) t2 = do
      -- v1 should not be bound since it is implicit.
      r <- lift Env.getRefMap
      im <- lift Env.getImplicitMap
      b <- lift (hasBoundVar t2)
      when b (throwError $ "unable to unify implicit variable "
                           ++ quote (varName v1) ++ " with term\n"
                           ++ preTermToString im r defaultExprIndent t2 ++ "\n"
                           ++ "containing bound variable(s)")
      if IntSet.member (varId v1) (preTermVars r t2)
        then throwError $
                "unable to unify implicit variable "
                ++ quote (varName v1)
                ++ " with\n"
                ++ preTermToString im r defaultExprIndent t2 ++ "\n"
                ++ "cyclic equation"
        else do
          return (IntMap.singleton (varId v1) t2)
    unifyVar t1 (TermVar True v2) = do
      -- v2 should not be bound, since it is implicit.
      r <- lift Env.getRefMap
      im <- lift Env.getImplicitMap
      b <- lift (hasBoundVar t1)
      when b (throwError $ "unable to unify implicit variable "
                           ++ quote (varName v2) ++ " with term\n"
                           ++ preTermToString im r defaultExprIndent t1 ++ "\n"
                           ++ "containing bound variable(s)")
      if IntSet.member (varId v2) (preTermVars r t1)
        then throwError $
                "unable to unify implicit variable "
                ++ quote (varName v2) ++ " with\n"
                ++ preTermToString im r defaultExprIndent t1 ++ "\n"
                ++ "cyclic equation"
        else do
          return (IntMap.singleton (varId v2) t1)
    unifyVar _ _ = throwError ""

    unifyAlpha :: Monad m => PreTerm -> PreTerm -> ExprUnifResult m
    unifyAlpha t1 t2 = do
      r <- lift Env.getRefMap
      if preTermsAlphaEqual r t1 t2
        then return IntMap.empty
        else throwUnableToUnify t1 t2
    
    throwUnableToUnify :: Monad m => PreTerm -> PreTerm -> ExprUnifResult m
    throwUnableToUnify t1 t2 = do
      r <- lift Env.getRefMap
      im <- lift Env.getImplicitMap
      throwError $ "unable to unify\n"
                   ++ preTermToString im r defaultExprIndent t1 ++ "\n"
                   ++ "with\n"
                   ++ preTermToString im r defaultExprIndent t2

mergeExprUnifMaps :: Monad m =>
  VarId -> SubstMap -> SubstMap -> ExprUnifResult m
mergeExprUnifMaps boundids m1 m2 =
  foldlM insertSubstMap m2 (IntMap.toList m1)
  where
    insertSubstMap :: Monad m =>
      SubstMap -> (VarId, PreTerm) -> ExprUnifResult m
    insertSubstMap m (i, t1) = do
      case IntMap.lookup i m of
        Nothing -> return (IntMap.insert i t1 m)
        Just t2 -> do
          m' <- exprUnifWithBoundIds True boundids t1 t2
          mergeExprUnifMaps boundids m' m
