{-# LANGUAGE BangPatterns #-}

module TypeCheck.ExprUnify
  ( ExprUnifResult
  , tcExprUnify
  , tcExprSubstUnify
  , runExprUnifResult
  , mergeExprUnifMaps
  )
where

import Str (quote)
import Loc
import TypeCheck.Env as Env
import TypeCheck.TypeCheckIO
import TypeCheck.Term
import TypeCheck.TermEnv
import TypeCheck.SubstMap

import Data.Foldable (foldlM)
import Control.Monad.Except
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromJust)
import Data.List (sortOn)
import Control.Exception (assert)

import Debug.Trace (trace)

_useTrace :: String -> a -> a
_useTrace = trace

type ExprUnifResult = ExceptT String TypeCheckIO SubstMap

tcExprSubstUnify :: Loc -> PreTerm -> PreTerm -> ExprIO ()
tcExprSubstUnify lo expectedTy actualTy = do
  su0 <- getExprSubst
  tcExprUnify lo (substPreTerm su0 expectedTy) (substPreTerm su0 actualTy)
  return ()

tcExprUnify ::
  Loc -> PreTerm -> PreTerm -> ExprIO ()
tcExprUnify lo t1 t2 = do
  runExprUnifResult lo True msgPrefix t1 t2
  where
    msgPrefix :: ExprIO String
    msgPrefix = do
      isu <- getExprSubst
      let t1' = substPreTerm isu t1
      let t2' = substPreTerm isu t2
      s1 <- lift $ preTermToString defaultExprIndent t1'
      s2 <- lift $ preTermToString defaultExprIndent t2'
      return $
        "expected expression to have type\n"
        ++ s1 ++ "\nbut type is\n" ++ s2

runExprUnifResult ::
  Loc -> Bool -> ExprIO String -> PreTerm -> PreTerm -> ExprIO ()
runExprUnifResult lo normalize msgPrefix t1 t2 = do
  r <- lift (runExceptT (exprUnifyUnnormalizedFirst normalize t1 t2))
  case r of
    Right m -> do
      isu <- getExprSubst
      isu' <- lift (runExceptT (mergeExprUnifMaps m isu))
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

canAppUnifyBase :: PreTerm -> Bool
canAppUnifyBase (TermVar True _) = False
canAppUnifyBase (TermVar False _) = True
canAppUnifyBase (TermData _) = True
canAppUnifyBase (TermCtor _ _) = True
canAppUnifyBase (TermRef _ _) = True
canAppUnifyBase (TermApp _ f _) = canAppUnifyBase f
canAppUnifyBase (TermLazyApp _ f) = canAppUnifyBase f
canAppUnifyBase (TermImplicitApp _ f _) = canAppUnifyBase f
canAppUnifyBase (TermLazyFun _ f) = canAppUnifyBase f
canAppUnifyBase (TermFun _ _ _ (CaseLeaf _ _ f _)) = canAppUnifyBase f
canAppUnifyBase (TermFun _ _ _ _) = False
canAppUnifyBase (TermCase _ _) = False
canAppUnifyBase (TermArrow _ _ _) = False
canAppUnifyBase (TermLazyArrow _ _) = False
canAppUnifyBase TermUnitElem = False
canAppUnifyBase TermUnitTy = False
canAppUnifyBase TermTy = False
canAppUnifyBase TermEmpty = False

canAppUnify :: RefMap -> PreTerm -> PreTerm -> Bool
canAppUnify rm t1 t2 =
  (canAppUnifyBase t1 && canAppUnifyBase t2)
  || preTermsAlphaEqual rm t1 t2

exprUnifyUnnormalizedFirst ::
  Bool -> PreTerm -> PreTerm -> ExprUnifResult
exprUnifyUnnormalizedFirst False t1 t2 =
  doExprUnify False t1 t2
exprUnifyUnnormalizedFirst True t1 t2 =
  catchError
    (doExprUnify False t1 t2)
    (\_ -> doExprUnify True t1 t2)

doExprUnify ::
  Bool -> PreTerm -> PreTerm -> ExprUnifResult
doExprUnify normalize = \t1 t2 -> do
  r <- lift Env.getRefMap
  --i0 <- lift Env.getImplicitMap
  --let !_ = trace ("expr unify " ++ preTermToString i0 r 0 (preTermNormalize r t1) ++ " with " ++ preTermToString i0 r 0 (preTermNormalize r t2)) ()
  if normalize
  then eunify2 (preTermNormalize r t1) (preTermNormalize r t2)
  else eunify2 t1 t2
  where
    eunify :: [(PreTerm, PreTerm)] -> SubstMap -> ExprUnifResult
    eunify = \ps su -> do
      rm <- lift Env.getRefMap
      iv <- lift Env.getImplicitVarMap
      let ps' = sortOn snd (map (addOrder rm iv) ps)
      startEunify (map fst ps') su
      where
        addOrder ::
          RefMap -> Env.ImplicitVarMap ->
          (PreTerm, PreTerm) -> ((PreTerm, PreTerm), (Int, Int, Int))
        addOrder rm iv (x, y) =
          let w1 = getAppAlphaArgumentWeight rm iv x
              w2 = getAppAlphaArgumentWeight rm iv y
          in ((x, y), min w1 w2)

        startEunify ::
          [(PreTerm, PreTerm)] -> SubstMap -> ExprUnifResult
        startEunify termPairs = \su0 -> do
          (msg, rest, subst) <- doEunify termPairs su0
          if null rest
          then return subst
          else throwError msg
          {-
            if length rest == length termPairs
            then throwError msg
            else do
              rest' <- lift (substPairs rest subst)
              eunify rest' subst
          -}

        doEunify ::
          [(PreTerm, PreTerm)] -> SubstMap ->
          ExceptT String TypeCheckIO (String, [(PreTerm, PreTerm)], SubstMap)
        doEunify [] su0 = return ("", [], su0)
        doEunify ((t1, t2) : ts) su0 =
          catchError
            (do
              m1 <- eunify2 t1 t2
              su1 <- mergeExprUnifMaps su0 m1
              ts' <- lift (substPairs ts su1)
              (er, rs, m2) <- doEunify ts' su1
              su2 <- mergeExprUnifMaps su1 m2
              return (er, rs, su2))
            (\msg -> do
              (_, rs, su) <- doEunify ts su0
              return (msg, (t1, t2) : rs, su))

    substPairs ::
      [(PreTerm, PreTerm)] -> SubstMap -> TypeCheckIO [(PreTerm, PreTerm)]
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

    eunify2 :: PreTerm -> PreTerm -> ExprUnifResult
    eunify2 t1 t2 =
      case (varBaseTerm t1, varBaseTerm t2) of
        (Just _, _) -> unifyVarApp t1 t2
        (_, Just _) -> unifyVarApp t1 t2
        _ -> eunify2NotVar t1 t2

    eunify2NotVar :: PreTerm -> PreTerm -> ExprUnifResult
    eunify2NotVar ar1@(TermArrow io1 d1 c1) ar2@(TermArrow io2 d2 c2) = do
      if io2 && not io1
      then do
        s1 <- lift $ preTermToString defaultExprIndent ar1
        s2 <- lift $ preTermToString defaultExprIndent ar2
        throwError $
          "unable to coerce effectful function type\n"
          ++ s2 ++ "\nto regular function type\n" ++ s1
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
              mergeExprUnifMaps m u)
            (\_ -> unifyAlpha ar1 ar2)
      where
        updateArrowMap ::
          SubstMap -> ((Maybe Var, PreTerm), (Maybe Var, PreTerm)) ->
          ExprUnifResult
        updateArrowMap m ((Nothing, _), (Nothing, _)) = return m
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
      if io2 && not io1
      then do
        s1 <- lift $ preTermToString defaultExprIndent (TermLazyArrow io1 c1)
        s2 <- lift $ preTermToString defaultExprIndent (TermLazyArrow io2 c2)
        throwError $
            "unable to assign effectful lazy type\n"
            ++ s2 ++ "\nto regular lazy type\n" ++ s1
      else eunify2 c1 c2
    eunify2NotVar t1@(TermApp _ f1 x1) t2@(TermApp _ f2 x2) = do
      rm <- lift Env.getRefMap
      if canAppUnify rm f1 f2
      then do
        s1 <- eunify2 f1 f2
        if length x1 /= length x2
          then do
            u1 <- lift $ preTermToString defaultExprIndent t1
            u2 <- lift $ preTermToString defaultExprIndent t2
            throwError $
                "unable to unify\n"
                ++ u1 ++ "\nwith\n" ++ u2 ++ "\ndifferent arities"
          else eunify (zip x1 x2) s1
      else unifyAlpha t1 t2
    eunify2NotVar t1@(TermImplicitApp _ f1 x1) t2@(TermImplicitApp _ f2 x2) = do
      rm <- lift Env.getRefMap
      if canAppUnify rm f1 f2
      then do
        s1 <- eunify2 f1 f2
        if length x1 /= length x2
          then do
            u1 <- lift $ preTermToString defaultExprIndent t1
            u2 <- lift $ preTermToString defaultExprIndent t2
            throwError $
              "unable to unify\n"
              ++ u1 ++ "\nwith\n" ++ u2 ++ "\ndifferent implicit arities"
          else do
            let z = map (\(a, b) -> (snd a, snd b)) (zip x1 x2)
            eunify z s1
      else unifyAlpha t1 t2
    eunify2NotVar t1@(TermLazyApp _ f1) t2@(TermLazyApp _ f2) = do
      rm <- lift Env.getRefMap
      if canAppUnify rm f1 f2
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

    makeNewVarIds :: [Var] -> TypeCheckIO ([PreTerm], SubstMap)
    makeNewVarIds [] = return ([], IntMap.empty)
    makeNewVarIds (i:is) = do
      (vs, su) <- makeNewVarIds is
      i' <- Env.freshVarId
      l <- Env.getNextLocalVarName
      let v = TermVar False (mkVar i' (varName i ++ "_" ++ l))
      let vs' = v : vs
      let su' = IntMap.insert (varId i) v su
      return (vs', su')

    isBoundVar :: ImplicitVarMap -> Var -> VarId -> Bool
    isBoundVar iv v i = do
      case IntMap.lookup i iv of
        Nothing -> i > varId v
        Just (_, di) ->
          case IntMap.lookup (varId v) iv of
            Nothing -> error ("expected " ++ varName v ++ " to be implicit var")
            Just (_, dv) -> not (head di `elem` dv)

    hasBoundVar :: Var -> PreTerm -> TypeCheckIO Bool
    hasBoundVar v t = do
      iv <- Env.getImplicitVarMap
      r <- Env.getRefMap
      return (preTermExistsVar r (isBoundVar iv v) t)

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

    unifyVarApp :: PreTerm -> PreTerm -> ExprUnifResult
    unifyVarApp t1 t2 =
      catchError (doUnifyVarApp t1 t2)
        (\_ ->
          catchError (unifyVar t1 t2)
            (\msg -> if msg == "" then unifyAlpha t1 t2 else throwError msg))

    doUnifyVarApp :: PreTerm -> PreTerm -> ExprUnifResult
    doUnifyVarApp t1@(TermVar True _) t2 = unifyVar t1 t2
    doUnifyVarApp t1 t2@(TermVar True _) = unifyVar t1 t2
    doUnifyVarApp (TermApp _ f1 []) (TermApp _ f2 []) =
      doUnifyVarApp f1 f2
    doUnifyVarApp t1@(TermApp io1 f1 as1) t2@(TermApp io2 f2 as2) =
      tryUnifyAll `catchError` \_ -> do
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
      where
        tryUnifyAll :: ExprUnifResult
        tryUnifyAll
          | length as1 == length as2 = do
              im <- lift Env.getImplicitMap
              rm <- lift Env.getRefMap
              iv <- lift Env.getImplicitVarMap
              if typesEqual im rm iv f1 f2
              then recUnify
              else throwError ""
          | True = throwError ""
          where
            recUnify = do
              s1 <- doUnifyVarApp f1 f2
              foldlM (\s (a1, a2)->
                        eunify2 a1 a2 >>= mergeExprUnifMaps s
                     ) s1 (zip as1 as2) 
            typesEqual im rm iv x y = 
              let s = preTermGetAlphaType im rm iv x
                  t = preTermGetAlphaType im rm iv y
              in case (s, t) of
                  (Just s', Just t') -> preTermsEqual rm s' t'
                  _ -> False
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

    unifyVar :: PreTerm -> PreTerm -> ExprUnifResult
    unifyVar (TermVar b1 v1) (TermVar b2 v2)
      | varId v1 == varId v2 = return IntMap.empty
      | b1 && b2 && varId v1 < varId v2 = do
          return (IntMap.singleton (varId v2) (TermVar b1 v1))
      | b1 && b2 && varId v2 < varId v1 = do
          return (IntMap.singleton (varId v1) (TermVar b2 v2))
      | b1 && not b2 = do
          when (varId v2 > varId v1) $
            throwError $
              "unable to unify implicit variable " ++ quote (varName v1)
              ++ " with variable " ++ quote (varName v2) ++ " not in other scope"
          return (IntMap.singleton (varId v1) (TermVar b2 v2))
      | not b1 && b2 = do
          when (varId v1 > varId v2) $
            throwError $
              "unable to unify implicit variable " ++ quote (varName v2)
              ++ " with variable " ++ quote (varName v1) ++ " not in other scope"
          return (IntMap.singleton (varId v2) (TermVar b1 v1))
      | True =
          throwError $
            "unable to unify distinct variables "
            ++ quote (varName v1) ++ " and " ++ quote (varName v2)
    unifyVar (TermVar True v1) t2 = do
      -- v1 should not be bound since it is implicit.
      b <- lift (hasBoundVar v1 t2)
      when b $ do
        s2 <- lift $ preTermToString defaultExprIndent t2
        throwError $
          "unable to unify implicit variable "
           ++ quote (varName v1) ++ " with term\n"
           ++ s2 ++ "\ncontaining variable(s) from other scope"
      r <- lift Env.getRefMap
      if IntSet.member (varId v1) (preTermVars r t2)
        then do
          s2 <- lift $ preTermToString defaultExprIndent t2
          throwError $
                "unable to unify implicit variable "
                ++ quote (varName v1)
                ++ " with\n" ++ s2 ++ "\ncyclic equation"
        else do
          return (IntMap.singleton (varId v1) t2)
    unifyVar t1 (TermVar True v2) = do
      -- v2 should not be bound, since it is implicit.
      b <- lift (hasBoundVar v2 t1)
      when b $ do
        s1 <- lift $ preTermToString defaultExprIndent t1
        throwError $
          "unable to unify implicit variable "
           ++ quote (varName v2) ++ " with term\n"
           ++ s1
           ++ "\ncontaining variable(s) from other scope"
      r <- lift Env.getRefMap
      if IntSet.member (varId v2) (preTermVars r t1)
        then do
          s1 <- lift $ preTermToString defaultExprIndent t1
          throwError $
            "unable to unify implicit variable "
            ++ quote (varName v2) ++ " with\n"
            ++ s1 ++ "\ncyclic equation"
        else do
          return (IntMap.singleton (varId v2) t1)
    unifyVar _ _ = throwError ""

    unifyAlpha :: PreTerm -> PreTerm -> ExprUnifResult
    unifyAlpha t1 t2 = do
      r <- lift Env.getRefMap
      if preTermsAlphaEqual r t1 t2
        then return IntMap.empty
        else throwUnableToUnify t1 t2
    
    throwUnableToUnify :: PreTerm -> PreTerm -> ExprUnifResult
    throwUnableToUnify t1 t2 = do
      s1 <- lift $ preTermToString defaultExprIndent t1
      s2 <- lift $ preTermToString defaultExprIndent t2
      throwError $ "unable to unify\n"
                   ++ s1 ++ "\nwith\n" ++ s2

makeFunWithVarSubst :: Bool -> [Var] -> PreTerm -> TypeCheckIO PreTerm
makeFunWithVarSubst isIo vs t = do
  vs' <- mapM (\v -> fmap (flip mkVar (varName v)) Env.freshVarId) vs
  let su = IntMap.fromList (zip (map varId vs) (map (TermVar False) vs'))
  let ct = CaseLeaf vs' isIo (substPreTerm su t) []
  return (TermFun [] isIo (Just (length vs)) ct)

mergeExprUnifMaps ::
  SubstMap -> SubstMap -> ExprUnifResult
mergeExprUnifMaps m1 m2 =
  foldlM insertSubstMap m2 (IntMap.toList m1)
  where
    insertSubstMap ::
      SubstMap -> (VarId, PreTerm) -> ExprUnifResult
    insertSubstMap m (i, t1) = do
      case IntMap.lookup i m of
        Nothing -> return (IntMap.insert i t1 m)
        Just t2 -> do
          m' <- exprUnifyUnnormalizedFirst True t1 t2
          mergeExprUnifMaps m' m
