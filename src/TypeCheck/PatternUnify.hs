{-# LANGUAGE BangPatterns #-}

module TypeCheck.PatternUnify
  ( PatUnifError (..)
  , PatUnifResult
  , tcPatternUnify
  , patternUnify2
  , patternUnify2NoNormalize
  , patUnifWithBoundIds
  , tcMergePatUnifMaps
  , mergePatUnifMaps
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

import Debug.Trace (trace)

_useTrace :: String -> a -> a
_useTrace = trace

data PatUnifError = UnifyUnable String | UnifyAbsurd String

type PatUnifResult = ExceptT PatUnifError TypeCheckIO SubstMap

runPatUnifResult ::
  Loc -> Maybe (PreTerm, PreTerm) -> PatUnifResult -> TypeCheckIO SubstMap
runPatUnifResult lo ts x = do
  r <- runExceptT x
  case r of
    Right m -> return m
    Left (UnifyUnable msg) -> do
      pfx <- msgPrefix
      err lo (Recoverable (pfx ++ "\n" ++ msg))
    Left (UnifyAbsurd msg) -> do
      pfx <- msgPrefix
      err lo (Recoverable $ pfx ++ "\n" ++ msg ++ "\ncase is absurd")
  where
    msgPrefix :: TypeCheckIO String
    msgPrefix = do
      case ts of
        Nothing -> return ""
        Just (t1, t2) -> do
          t1' <- preTermToString defaultExprIndent t1
          t2' <- preTermToString defaultExprIndent t2
          return $
            "failed matching type\n" ++ t1'
            ++ "\nwith type\n" ++ t2'

tcPatternUnify ::
  VarId -> Loc -> PreTerm -> PreTerm -> TypeCheckIO SubstMap
tcPatternUnify newpids lo t1 t2 =
  runPatUnifResult lo (Just (t1, t2)) (patternUnify2 newpids t1 t2)

patternUnify2 :: VarId -> PreTerm -> PreTerm -> PatUnifResult
patternUnify2 newPatternIds t1 t2 = do
  boundIds <- lift Env.getNextVarId
  patUnifWithBoundIds True newPatternIds boundIds t1 t2

patternUnify2NoNormalize ::
  VarId -> PreTerm -> PreTerm -> PatUnifResult
patternUnify2NoNormalize newPatternIds t1 t2 = do
  boundIds <- lift Env.getNextVarId
  patUnifWithBoundIds False newPatternIds boundIds t1 t2

canAppUnify :: PreTerm -> Bool
canAppUnify (TermApp _ f _) = canAppUnify f
canAppUnify (TermLazyApp _ f) = canAppUnify f
canAppUnify (TermImplicitApp _ f _) = canAppUnify f
canAppUnify (TermData _) = True
canAppUnify (TermCtor _ _) = True
canAppUnify _ = False

patUnifWithBoundIds ::
  Bool -> VarId -> VarId -> PreTerm -> PreTerm -> PatUnifResult
patUnifWithBoundIds False newPatternIds boundIds t1 t2 =
  doPatUnifWithBoundIds False newPatternIds boundIds t1 t2
patUnifWithBoundIds True newPatternIds boundIds t1 t2 =
  catchError
    (doPatUnifWithBoundIds False newPatternIds boundIds t1 t2)
    (\_ -> doPatUnifWithBoundIds True newPatternIds boundIds t1 t2)

doPatUnifWithBoundIds ::
  Bool -> VarId -> VarId -> PreTerm -> PreTerm -> PatUnifResult
doPatUnifWithBoundIds withNormalize newPatternIds boundIds = \t1 t2 -> do
  r <- lift Env.getRefMap
  --im0 <- lift Env.getImplicitMap
  --let !() = trace ("pattern unify " ++ preTermToString im0 r 0 t1 ++ " with " ++ preTermToString im0 r 0 t2) ()
  if withNormalize
  then punify2 (preTermNormalize r t1) (preTermNormalize r t2)
  else punify2 t1 t2
  where
    punify :: [(PreTerm, PreTerm)] -> SubstMap -> PatUnifResult
    punify termPairs = \su0 -> do
      (msg, rest, subst) <- doPunify termPairs su0
      if null rest
      then return subst
      else
        if length rest == length termPairs
        then throwError (UnifyUnable msg)
        else do
          rest' <- lift (substPairs rest subst)
          punify rest' subst
      where
        doPunify ::
          [(PreTerm, PreTerm)] -> SubstMap ->
          ExceptT PatUnifError TypeCheckIO
            (String, [(PreTerm, PreTerm)], SubstMap)
        doPunify [] su0 = return ("", [], su0)
        doPunify ((t1, t2) : ts) su0 =
          catchError
            (do
              m1 <- punify2 t1 t2
              su1 <- mergePatUnifMaps newPatternIds boundIds su0 m1
              ts' <- lift (substPairs ts su1)
              (er, rs, m2) <- doPunify ts' su1
              su2 <- mergePatUnifMaps newPatternIds boundIds su1 m2
              return (er, rs, su2))
            (\er ->
              case er of
                UnifyUnable msg -> do
                  (_, rs, su) <- doPunify ts su0
                  return (msg, (t1, t2) : rs, su)
                UnifyAbsurd msg ->
                  throwError (UnifyAbsurd msg))

    substPairs ::
      [(PreTerm, PreTerm)] -> SubstMap -> TypeCheckIO [(PreTerm, PreTerm)]
    substPairs ps m = do
      r <- Env.getRefMap
      if withNormalize
      then
        return $
          map (\(x,y) -> (preTermNormalize r (substPreTerm m x),
                          preTermNormalize r (substPreTerm m y))) ps
      else
        return $
          map (\(x,y) -> (substPreTerm m x, substPreTerm m y)) ps

    punify2 :: PreTerm -> PreTerm -> PatUnifResult
    punify2 t1 t2 =
      case (varBaseTerm' t1, varBaseTerm' t2) of
        (Just (_, _), _) -> unifyVarApp t1 t2
        (_, Just (_, _)) -> unifyVarApp t1 t2
        _ -> punify2NotVar t1 t2

    punify2NotVar :: PreTerm -> PreTerm -> PatUnifResult
    punify2NotVar ar1@(TermArrow io1 d1 c1) ar2@(TermArrow io2 d2 c2) = do
      if io1 /= io2
      then throwError . UnifyAbsurd $
            "unable to unify effectful function type with regular function type"
      else if length d1 /= length d2
        then do
          throwError . UnifyAbsurd $ "function types with different arities"
        else
          catchError (
            do
              let ds = zip d1 d2
              su <- foldlM updateArrowMap IntMap.empty ds
              let ds' = map (\(x,y) -> (substPreTerm su (snd x),
                                        substPreTerm su (snd y))) ds
              r <- lift Env.getRefMap
              m <- punify ds' IntMap.empty
              u <- if withNormalize
                   then punify2
                          (preTermNormalize r (substPreTerm m (substPreTerm su c1)))
                          (preTermNormalize r (substPreTerm m (substPreTerm su c2)))
                   else punify2
                          (substPreTerm m (substPreTerm su c1))
                          (substPreTerm m (substPreTerm su c2))
              mergePatUnifMaps newPatternIds boundIds m u)
            (unifyAlphaIfUnable ar1 ar2)
      where
        updateArrowMap ::
          SubstMap -> ((Maybe Var, PreTerm), (Maybe Var, PreTerm)) ->
          PatUnifResult
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
    punify2NotVar (TermLazyArrow io1 c1) (TermLazyArrow io2 c2) = do
      if io1 /= io2
      then throwError . UnifyAbsurd $
            "unable to unify effectful lazy type with regular lazy type"
      else
        punify2 c1 c2
    punify2NotVar t1@(TermApp _ f1 x1) t2@(TermApp _ f2 x2) =
      if canAppUnify f1 && canAppUnify f2
      then do
        s1 <- punify2 f1 f2
        if length x1 /= length x2
          then do
            t1' <- lift $ preTermToString defaultExprIndent t1
            t2' <- lift $ preTermToString defaultExprIndent t2
            throwError . UnifyAbsurd $
              "unable to unify\n"
              ++ t1' ++ "\nwith\n" ++ t2' ++ "\ndifferent arities"
          else punify (zip x1 x2) s1
      else unifyAlpha t1 t2
    punify2NotVar t1@(TermImplicitApp _ f1 x1) t2@(TermImplicitApp _ f2 x2) =
      if canAppUnify f1 && canAppUnify f2
      then do
        s1 <- punify2 f1 f2
        if length x1 /= length x2
          then do
            t1' <- lift $ preTermToString defaultExprIndent t1
            t2' <- lift $ preTermToString defaultExprIndent t2
            throwError . UnifyAbsurd $
                  "unable to unify\n"
                  ++ t1' ++ "\nwith\n" ++ t2'
                  ++ "\ndifferent implicit arities"
          else do
            let z = map (\(a, b) -> (snd a, snd b)) (zip x1 x2)
            punify z s1
      else unifyAlpha t1 t2
    punify2NotVar t1@(TermLazyApp _ f1) t2@(TermLazyApp _ f2) = do
      if canAppUnify f1 && canAppUnify f2
        then punify2 f1 f2
        else unifyAlpha t1 t2
    punify2NotVar (TermCtor v1 _) (TermCtor v2 _) =
      if varId v1 == varId v2
        then return IntMap.empty
        else throwError . UnifyAbsurd $
                "cannot unify different constructors "
                ++ quote (varName v1) ++ " and " ++ quote (varName v2)
    punify2NotVar (TermData v1) (TermData v2) =
      if varId v1 == varId v2
        then return IntMap.empty
        else throwError . UnifyAbsurd $
                "cannot unify different data types "
                ++ quote (varName v1) ++ " and " ++ quote (varName v2)
    punify2NotVar TermTy TermTy = return IntMap.empty
    punify2NotVar TermUnitElem TermUnitElem = return IntMap.empty
    punify2NotVar TermUnitTy TermUnitTy = return IntMap.empty
    punify2NotVar f1@(TermLazyFun io1 t1) f2@(TermLazyFun io2 t2) = do
      if io1 /= io2
      then throwError . UnifyAbsurd $
            "unable to unify effectful lazy with pure lazy"
      else
        if io1
        then
          unifyAlpha f1 f2
        else
          catchError (punify2 t1 t2) (unifyAlphaIfUnable f1 f2)
    punify2NotVar f1@(TermFun ias1 io1 _ (CaseLeaf i1 t1 _))
                           f2@(TermFun ias2 io2 _ (CaseLeaf i2 t2 _))
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
              punify2 t1' t2')
            (unifyAlphaIfUnable f1 f2)
      | io1 /= io2 =
          throwError . UnifyAbsurd $
            "unable to unify effectful function with regular function"
      | length i1 /= length i2 =
          throwError . UnifyAbsurd $
            "unable to unify functions with different arities"
      | True =
          throwError . UnifyAbsurd $
            "unable to unify values with different implicits"
    punify2NotVar f1@(TermLazyFun False t1) t2 =
      catchError (punify2 t1 (TermLazyApp False t2))
        (unifyAlphaIfUnable f1 t2)
    punify2NotVar t1 f2@(TermLazyFun False t2) =
      catchError (punify2 (TermLazyApp False t1) t2)
        (unifyAlphaIfUnable t1 f2)
    punify2NotVar f1@(TermFun [] False _ (CaseLeaf i1 t1 _)) t2 = do
      catchError
        (do
          (vs, su) <- lift (makeNewVarIds i1)
          let t1' = substPreTerm su t1
          punify2 t1' (TermApp False t2 vs))
        (unifyAlphaIfUnable f1 t2)
    punify2NotVar t1 f2@(TermFun [] False _ (CaseLeaf i2 t2 _)) = do
      catchError
        (do
          (vs, su) <- lift (makeNewVarIds i2)
          let t2' = substPreTerm su t2
          punify2 (TermApp False t1 vs) t2')
        (unifyAlphaIfUnable t1 f2)
    punify2NotVar t1 t2 = unifyAlphaCheckRigid t1 t2

    makeNewVarIds :: [Var] -> TypeCheckIO ([PreTerm], SubstMap)
    makeNewVarIds [] = return ([], IntMap.empty)
    makeNewVarIds (i:is) = do
      (vs, su) <- makeNewVarIds is
      i' <- Env.freshVarId
      let v = TermVar False (mkVar i' ('#' : varName i))
      let vs' = v : vs
      let su' = IntMap.insert (varId i) v su
      return (vs', su')

    isBoundVar :: VarId -> Bool
    isBoundVar i = i >= boundIds

    hasBoundVar :: PreTerm -> TypeCheckIO Bool
    hasBoundVar t = do
      r <- Env.getRefMap
      return (preTermExistsVar r isBoundVar t)

    allBoundVars :: [PreTerm] -> Maybe [Var]
    allBoundVars [] = Just []
    allBoundVars (TermVar _ v : ts)
      | isBoundVar (varId v) = fmap (v:) (allBoundVars ts)
      | True = Nothing
    allBoundVars (_:_) = Nothing

    varBaseTerm :: PreTerm -> Maybe Var
    varBaseTerm (TermApp _ f _) = varBaseTerm f
    varBaseTerm (TermLazyApp _ f) = varBaseTerm f
    varBaseTerm (TermVar False v) = Just v
    varBaseTerm (TermVar True _) = Nothing
    varBaseTerm _ = Nothing

    varBaseTerm' :: PreTerm -> Maybe (Bool, Var)
    varBaseTerm' (TermApp _ f _) = varBaseTerm' f
    varBaseTerm' (TermLazyApp _ f) = varBaseTerm' f
    varBaseTerm' (TermVar b v) = Just (b, v)
    varBaseTerm' _ = Nothing

    varBaseBoundVars :: PreTerm -> [PreTerm] -> Maybe (Var, [Var])
    varBaseBoundVars p ts = do
      v <- varBaseTerm p
      vs <- allBoundVars ts
      Just (v, vs)

    unifyVarApp :: PreTerm -> PreTerm -> PatUnifResult
    unifyVarApp t1 t2 =
      catchError (doUnifyVarApp t1 t2)
        (\_ ->
          catchError (unifyVar t1 t2)
            (\msg ->
              case msg of
                UnifyUnable "" -> unifyAlpha t1 t2
                _ -> throwError msg))

    doUnifyVarApp :: PreTerm -> PreTerm -> PatUnifResult
    doUnifyVarApp t1@(TermVar _ _) t2 =
      unifyVar t1 t2 `catchError` (\_ -> doUnifyVarApp' t1 t2)
    doUnifyVarApp t1 t2@(TermVar _ _) =
      unifyVar t1 t2 `catchError` (\_ -> doUnifyVarApp' t1 t2)
    doUnifyVarApp t1 t2 = doUnifyVarApp' t1 t2

    doUnifyVarApp' :: PreTerm -> PreTerm -> PatUnifResult
    doUnifyVarApp' t1@(TermVar False _) t2 = unifyVar t1 t2
    doUnifyVarApp' t1 t2@(TermVar False _) = unifyVar t1 t2
    doUnifyVarApp' (TermApp _ f1 []) (TermApp _ f2 []) =
      doUnifyVarApp f1 f2
    doUnifyVarApp' t1@(TermApp io1 f1 as1) t2@(TermApp io2 f2 as2) =
      case varBaseBoundVars f1 as1 of
        Just (v1, vs1) ->
          case varBaseBoundVars f2 as2 of
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
          case varBaseBoundVars f2 as2 of
            Just (_, vs2) -> do
              f1' <- lift (makeFunWithVarSubst io1 vs2 t1)
              doUnifyVarApp f1' f2
            Nothing -> throwError (UnifyUnable "")
    doUnifyVarApp' (TermApp io f1 as1) t2 =
      case varBaseBoundVars f1 as1 of
        Nothing -> throwError (UnifyUnable "")
        Just (_, vs1) -> do
          f2' <- lift (makeFunWithVarSubst io vs1 t2)
          doUnifyVarApp f1 f2'
    doUnifyVarApp' t1 (TermApp io f2 as2) =
      case varBaseBoundVars f2 as2 of
        Nothing -> throwError (UnifyUnable "")
        Just (_, vs2) -> do
          f1' <- lift (makeFunWithVarSubst io vs2 t1)
          doUnifyVarApp f1' f2
    doUnifyVarApp' (TermLazyApp _ f1) (TermLazyApp _ f2) =
      doUnifyVarApp f1 f2
    doUnifyVarApp' (TermLazyApp io f1) t2 =
      case varBaseTerm f1 of
        Nothing -> throwError (UnifyUnable "")
        Just _ ->
          doUnifyVarApp f1 (TermLazyFun io t2)
    doUnifyVarApp' t1 (TermLazyApp io f2) =
      case varBaseTerm f2 of
        Nothing -> throwError (UnifyUnable "")
        Just _ ->
          doUnifyVarApp (TermLazyFun io t1) f2
    doUnifyVarApp' t1 t2 = unifyVar t1 t2

    unifyVar :: PreTerm -> PreTerm -> PatUnifResult
    unifyVar (TermVar b1 v1) (TermVar b2 v2)
      | varId v1 == varId v2 = return IntMap.empty
      | isBoundVar (varId v1) && isBoundVar (varId v2) =
          throwError . UnifyUnable $
            "unable to unify distinct bound variables "
            ++ quote (varName v1) ++ " and " ++ quote (varName v2)
      | isBoundVar (varId v1) =
          throwError . UnifyUnable $
            "unable to unify variable " ++ quote (varName v1)
            ++" with bound variable " ++ quote (varName v1)
      | isBoundVar (varId v2) =
          throwError . UnifyUnable $
            "unable to unify variable " ++ quote (varName v1)
            ++ " with bound variable " ++ quote (varName v2)
      | b1 && b2 =
          throwError . UnifyUnable $
            "unable to unify distinct implicit variables "
            ++ quote (varName v1) ++ " and " ++ quote (varName v2)
      | b1 && not b2 =
          return (IntMap.singleton (varId v2) (TermVar True v1))
      | not b1 && b2 =
          return (IntMap.singleton (varId v1) (TermVar True v2))
      | varId v1 < newPatternIds && varId v2 >= newPatternIds =
          return (IntMap.singleton (varId v2) (TermVar b1 v1))
      | varId v2 < newPatternIds && varId v1 >= newPatternIds =
          return (IntMap.singleton (varId v1) (TermVar b2 v2))
      | varName v1 /= "_" && varName v2 == "_" =
          return (IntMap.singleton (varId v2) (TermVar b1 v1))
      | varName v1 == "_" && varName v2 /= "_" =
          return (IntMap.singleton (varId v1) (TermVar b2 v2))
      | varId v1 < varId v2 = do
          return (IntMap.singleton (varId v2) (TermVar b1 v1))
      | varId v1 > varId v2 = do
          return (IntMap.singleton (varId v1) (TermVar b2 v2))
    unifyVar (TermVar True v1) t2 = do
      r <- lift Env.getRefMap
      let fs = preTermVars r t2
      when (not (IntSet.null (IntSet.filter (>= newPatternIds) fs))) $ do
        t2' <- lift $ preTermToString defaultExprIndent t2
        throwError . UnifyUnable $
           "unable to unify implicit variable "
           ++ quote (varName v1) ++ " with\n" ++ t2'
      when (IntSet.member (varId v1) fs) $ do
        t2' <- lift $ preTermToString defaultExprIndent t2
        throwError . UnifyUnable $
          "unable to unify variable " ++ quote (varName v1)
          ++ " with\n" ++ t2' ++ "\ncyclic equation"
      return (IntMap.singleton (varId v1) t2)
    unifyVar t1 (TermVar True v2) = do
      r <- lift Env.getRefMap
      let fs = preTermVars r t1
      when (not (IntSet.null (IntSet.filter (>= newPatternIds) fs))) $ do
        t1' <- lift $ preTermToString defaultExprIndent t1
        throwError . UnifyUnable $
           "unable to unify implicit variable "
           ++ quote (varName v2) ++ " with\n" ++ t1'
      when (IntSet.member (varId v2) fs) $ do
        t1' <- lift $ preTermToString defaultExprIndent t1
        throwError . UnifyUnable $
          "unable to unify variable " ++ quote (varName v2)
          ++ " with\n" ++ t1' ++ "\ncyclic equation"
      return (IntMap.singleton (varId v2) t1)
    unifyVar (TermVar False v1) t2 = do
      when (isBoundVar (varId v1)) $ do
        t2' <- lift $ preTermToString defaultExprIndent t2
        throwError . UnifyUnable $
          "cannot unify bound variable " ++ quote (varName v1)
          ++ " with\n" ++ t2'
      b <- lift (hasBoundVar t2)
      when b $ do
        t2' <- lift $ preTermToString defaultExprIndent t2
        throwError . UnifyUnable $
            "unable to unify variable "
            ++ quote (varName v1) ++ " with term\n"
            ++ t2' ++ "\ncontaining bound variable(s)"
      r <- lift Env.getRefMap
      if IntSet.member (varId v1) (preTermVars r t2)
        then do
          t2' <- lift $ preTermToString defaultExprIndent t2
          throwError . UnifyUnable $
                "unable to unify variable " ++ quote (varName v1)
                ++ " with\n" ++ t2' ++ "\ncyclic equation"
        else return (IntMap.singleton (varId v1) t2)
    unifyVar t1 (TermVar False v2) = do
      when (isBoundVar (varId v2)) $ do
        t1' <- lift $ preTermToString defaultExprIndent t1
        throwError . UnifyUnable $
          "cannot unify bound variable " ++ quote (varName v2)
          ++ " with\n" ++ t1'
      b <- lift (hasBoundVar t1)
      when b $ do
        t1' <- lift $ preTermToString defaultExprIndent t1
        (throwError . UnifyUnable $
                "unable to unify variable "
                ++ quote (varName v2) ++ " with term\n"
                ++ t1' ++ "\ncontaining bound variable(s)")
      r <- lift Env.getRefMap
      if IntSet.member (varId v2) (preTermVars r t1)
        then do
          t1' <- lift $ preTermToString defaultExprIndent t1
          throwError . UnifyUnable $
              "unable to unify variable " ++ quote (varName v2)
              ++ " with\n"
              ++ t1' ++ "\ncyclic equation"
        else return (IntMap.singleton (varId v2) t1)
    unifyVar _ _ = throwError (UnifyUnable "")

    unifyAlphaCheckRigid ::
      PreTerm -> PreTerm -> PatUnifResult
    unifyAlphaCheckRigid t1 t2 = do
      if preTermIsRigid t1 && preTermIsRigid t2
        then do
          t1' <- lift $ preTermToString defaultExprIndent t1
          t2' <- lift $ preTermToString defaultExprIndent t2
          throwError . UnifyAbsurd $
              "cannot unify\n" ++ t1' ++ "\nwith\n" ++ t2'
        else unifyAlpha t1 t2

    unifyAlpha :: PreTerm -> PreTerm -> PatUnifResult
    unifyAlpha t1 t2 = do
      r <- lift Env.getRefMap
      if preTermsAlphaEqual r t1 t2
        then return IntMap.empty
        else throwUnableToUnify t1 t2

    unifyAlphaIfUnable ::
      PreTerm -> PreTerm -> PatUnifError -> PatUnifResult
    unifyAlphaIfUnable _ _ e@(UnifyAbsurd _) = throwError e
    unifyAlphaIfUnable t1 t2 (UnifyUnable _) = unifyAlpha t1 t2

    throwUnableToUnify ::
      PreTerm -> PreTerm -> PatUnifResult
    throwUnableToUnify t1 t2 = do
      t1' <- lift $ preTermToString defaultExprIndent t1
      t2' <- lift $ preTermToString defaultExprIndent t2
      throwError . UnifyUnable $
        "unable to unify\n" ++ t1' ++ "\nwith\n" ++ t2'

makeFunWithVarSubst :: Bool -> [Var] -> PreTerm -> TypeCheckIO PreTerm
makeFunWithVarSubst isIo vs t = do
  vs' <- mapM (\v -> fmap (flip mkVar (varName v)) Env.freshVarId) vs
  let su = IntMap.fromList (zip (map varId vs) (map (TermVar False) vs'))
  let ct = CaseLeaf vs' (substPreTerm su t) []
  return (TermFun [] isIo (Just (length vs)) ct)

mergePatUnifMaps ::
  VarId -> VarId -> SubstMap -> SubstMap -> PatUnifResult
mergePatUnifMaps newpids boundids m1 m2 =
  foldlM insertSubstMap m2 (IntMap.toList m1)
  where
    insertSubstMap ::
      SubstMap -> (VarId, PreTerm) -> PatUnifResult
    insertSubstMap m (i, t1) = do
      case IntMap.lookup i m of
        Nothing -> return (IntMap.insert i t1 m)
        Just t2 -> do
          m' <- patUnifWithBoundIds True newpids boundids t1 t2
          mergePatUnifMaps newpids boundids m' m

tcMergePatUnifMaps ::
  VarId -> Loc -> SubstMap -> SubstMap -> TypeCheckIO SubstMap
tcMergePatUnifMaps newpids lo m1 m2 = do
  boundids <- Env.getNextVarId
  runPatUnifResult lo Nothing (mergePatUnifMaps newpids boundids m1 m2)
