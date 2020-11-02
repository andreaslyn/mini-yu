{-# LANGUAGE BangPatterns #-}

module TypeCheck.TypeCheck (runTT)
where

import Str
import Loc (Loc)
import qualified Loc
import qualified TypeCheck.Env as Env
import ParseTree
import YuParser (runParse)
import TypeCheck.Term
import TypeCheck.TermEnv
import TypeCheck.Env
  (VarStatus (..), isStatusUnknown, isStatusInProgress, isStatusTerm)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Either (isRight)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import TypeCheck.SubstMap
import Data.List (find, sortOn, isPrefixOf)
import Data.Foldable (foldlM, foldrM)
import System.Directory
  (getCurrentDirectory, setCurrentDirectory, canonicalizePath,
   doesFileExist, getPermissions, readable)
import System.FilePath (takeDirectory)
import qualified System.FilePath.Posix as FilePath
import Control.Exception (assert)
import TypeCheck.TypeCheckT
import TypeCheck.PatternUnify
import TypeCheck.ExprUnify
import TypeCheck.TerminationCheck

import Data.List (intercalate)
import Debug.Trace (trace)

_useTrace :: String -> a -> a
_useTrace = trace

_useIntercalate :: [a] -> [[a]] -> [a]
_useIntercalate = intercalate

runTypeCheckT ::
  MonadIO m =>
  Bool -> FilePath -> FilePath -> Program ->
  m (Either String ([RefVar], DataCtorMap, ImplicitMap, RefMap))
runTypeCheckT compiling stdImport file prog = do
  r <- runExceptT
        (runReaderT
          (evalStateT
            (Env.runEnvT (tcInitProgram compiling stdImport prog))
              (Set.singleton file)) file)
  case r of
    Left e -> return (Left (typeCheckErrMsg e))
    Right x -> return (Right x)

importFilePath :: MonadIO m => FilePath -> FilePath -> m FilePath
importFilePath stdImport p
  | isPrefixOf "yu/" p = liftIO $ canonicalizePath (stdImport ++ p)
  | True = liftIO $ canonicalizePath p

runTT ::
  MonadIO m =>
  Bool -> FilePath -> FilePath ->
  m (Either String ([RefVar], DataCtorMap, ImplicitMap, RefMap))
runTT compiling stdImport file = do
  file' <- importFilePath stdImport file
  isf <- liftIO (doesFileExist file')
  if not isf
  then return (Left $ "unable to open file " ++ file)
  else do
    perms <- liftIO (getPermissions file')
    if not (readable perms)
    then return (Left $ "unable to read file " ++ file)
    else do
      prog <- runParse file'
      case prog of
        Left e -> return (Left e)
        Right prog' -> runTypeCheckT compiling stdImport file' prog'

varStatusTerm :: Monad m => VarStatus -> TypeCheckT m Term
varStatusTerm (StatusTerm te) = return te
varStatusTerm (StatusUnknownCtor subst depth n d) =
  Env.withDepth depth (tcDataDefCtorLookup subst n d)
varStatusTerm (StatusUnknownRef subst depth d) =
  Env.withDepth depth (tcDef subst False d)
varStatusTerm (StatusUnknownVar subst depth (lo, na) e) =
  case e of
    Nothing -> err lo $ Fatal ("cannot determine type of variable " ++ quote na)
    Just e' -> Env.withDepth depth (tcVar subst (lo, na) e')
varStatusTerm (StatusInProgress _ (lo, na)) =
  err lo $ Fatal (quote na ++ " has cyclic type")

verifyMultipleUsesIdent :: Monad m => Bool -> (Loc, VarName) -> TypeCheckT m ()
verifyMultipleUsesIdent True (lo, na) = 
  err lo (Fatal $ "multiple instances of identifier " ++ quote na ++ " in scope")
verifyMultipleUsesIdent False _ = return ()

insertUnknownCtor :: Monad m =>
  SubstMap -> (Loc, VarName) -> Decl -> TypeCheckT m ()
insertUnknownCtor subst da d = do
  let na = declName d
  let lo = declLoc d
  checkRefNameValid (lo, na)
  depth <- Env.getDepth
  x <- Env.tryInsert na (StatusUnknownCtor subst depth da d)
  verifyMultipleUsesIdent (isJust x) (lo, na)

insertUnknownRef :: Monad m => SubstMap -> Def -> TypeCheckT m ()
insertUnknownRef subst d = do
  let na = defName d
  let lo = defLoc d
  checkRefNameValid (lo, na)
  depth <- Env.getDepth
  x <- Env.tryInsert na (StatusUnknownRef subst depth d)
  verifyMultipleUsesIdent (isJust x) (lo, na)
  case d of
    DefData _ _ ctors -> mapM_ (insertUnknownCtor subst (lo, na)) ctors
    _ -> return ()

insertUnknownVar :: Monad m =>
  SubstMap -> (Loc, VarName) -> Maybe Expr -> TypeCheckT m ()
insertUnknownVar subst (lo, na) e = do
  depth <- Env.getDepth
  let s = StatusUnknownVar subst depth (lo, na) e
  if na == "_"
    then
      return ()
    else do
      checkVarNameValid (lo, na)
      x <- Env.tryInsert na s
      verifyMultipleUsesIdent (isJust x) (lo, na)

markInProgress :: Monad m => Bool -> (Loc, VarName) -> TypeCheckT m (Maybe Term)
markInProgress isCtor (lo, na) = do
  x <- Env.lookup na
  case x of
    Just (StatusTerm t) ->
      return (Just t)
    Just (StatusInProgress _ _) ->
      error $ "name already in progress " ++ quote na
    _ -> do
      y <- Env.tryUpdateIf isStatusUnknown na (StatusInProgress isCtor (lo, na))
      when (isNothing y)
            (error $ "expected unknown env status of " ++ quote na)
      return Nothing

updateToStatusTerm :: Monad m => VarName -> Term -> TypeCheckT m ()
updateToStatusTerm na te = do
  x <- Env.tryUpdateIf
        (\c -> isStatusInProgress c || isStatusTerm c) na (StatusTerm te)
  when (isNothing x) (error $ "expected in-progress or term env status of " ++ quote na)

insertNonblankFreshVariable :: Monad m => (Loc, VarName) -> PreTerm -> TypeCheckT m Var
insertNonblankFreshVariable (lo, na) te = do
  i <- Env.freshVarId
  let v = mkVar i na
  let v' = TermVar False v
  let y = StatusTerm (mkTerm v' te False)
  if na == "_"
    then
      return v
    else do
      checkVarNameValid (lo, na)
      x <- Env.tryInsert na y
      verifyMultipleUsesIdent (isJust x) (lo, na)
      return v

checkVarNameValid :: Monad m => (Loc, VarName) -> TypeCheckT m ()
checkVarNameValid (lo, na) = do
  when (isKeyword na)
          (err lo (Fatal $
                    "invalid variable name " ++ quote na
                    ++ ", it is a keyword"))
  when invalidName (err lo (Fatal $ "invalid variable name " ++ quote na))
  where
    invalidName :: Bool
    invalidName = '.' `elem` na || '\\' `elem` na || '_' `elem` na

checkRefNameValid :: Monad m => (Loc, VarName) -> TypeCheckT m ()
checkRefNameValid (lo, na0) = do
  when (isStrictKeyword (stripOperatorStr (head nas)))
          (err lo (Fatal $
                    "invalid identifier " ++ quote na0
                    ++ ", it is a strict keyword"))
  let le = length nas
  let (fnas, [lnas]) = splitAt (le - 1) nas
  let b = if le > 1 && lnas == ""
          then any invalidName fnas
          else any invalidName nas
  when b (err lo (Fatal $ "invalid identifier " ++ quote na0))
  where
    nas :: [VarName]
    nas = splitOn '\\' na0

    splitOn :: Char -> VarName -> [VarName]
    splitOn _ "" = [""]
    splitOn c (x : xs) =
      let s = splitOn c xs
      in if c == x
          then "" : s
          else (x : head s) : tail s

    invalidName :: VarName -> Bool
    invalidName n = null n || invalidNameSplit n

    invalidNameSplit :: VarName -> Bool
    invalidNameSplit n =
      let (h, n', t) = nameSplit n
      in h == '.' ||
          if h == '_'
          then
            if not (null n')
            then
              if isOperatorChar (head n')
              then t /= '_' || elem '_' n'
              else t == '_' || elem '_' n'
            else
              t == '_' || isOperatorChar t
          else
            if isOperatorChar h
            then t /= '_' || elem '_' n'
            else elem '_' n

    nameSplit :: VarName -> (Char, VarName, Char)
    nameSplit n =
      case n of
        "" -> error "empty identifier"
        h : n' ->
          case splitAt (length n' - 1) n' of
            (n'', [t]) -> (h, n'', t)
            _ -> (h, [h], h)

checkMainExists :: Monad m => TypeCheckT m ()
checkMainExists = do
  ma <- Env.lookup "main"
  case ma of
    Nothing ->
      err (Loc.loc 1 1) (Fatal $ "missing " ++ quote "main" ++ " function")
    Just (StatusTerm t) -> do
      im <- Env.getImplicitMap
      rm <- Env.getRefMap
      case preTermNormalize rm (termTy t) of
        TermArrow True [] TermUnitTy -> checkBody (termPre t)
        _ -> 
          err (Loc.loc 1 1)
            (Fatal $
              "expected type of " ++ quote "main" ++ " to be\n"
              ++ preTermToString im rm defaultExprIndent
                    (TermArrow True [] TermUnitTy))
    Just _ -> error "unexpected status of main function"
  where
    checkBody :: Monad m => PreTerm -> TypeCheckT m ()
    checkBody (TermRef v _) = do
      r <- Env.forceLookupRef (varId v)
      case termPre r of
        TermFun _ _ _ _ -> return ()
        _ -> invalidMainBody
    checkBody _ = invalidMainBody

    invalidMainBody :: Monad m => TypeCheckT m ()
    invalidMainBody =
      err (Loc.loc 1 1)
        (Fatal $
          "it is required that " ++ quote "main"
          ++ " is defined by let () => ...")

tcInitProgram :: MonadIO m =>
  Bool -> FilePath -> Program -> TypeCheckT m [RefVar]
tcInitProgram compiling stdImport prog = do
  origDir <- liftIO getCurrentDirectory
  rs <- doTcInitProgram
  liftIO (setCurrentDirectory origDir)
  return rs
  where
    doTcInitProgram :: MonadIO m => TypeCheckT m [RefVar]
    doTcInitProgram = do
      rs <- tcProgram stdImport prog
      mapM_ checkPositivity rs
      rm <- Env.getRefMap
      let ks = IntMap.keys rm
      mapM_ verifyRecursiveImpureChains ks
      mapM_ verifyDataImpureChains rs
      mapM_ verifyImpureIsNotTerminationChecked ks
      when compiling checkMainExists
      return rs

    checkPositivity :: Monad m => RefVar -> TypeCheckT m ()
    checkPositivity (RefData dv) = do
      ctors <- Env.forceLookupDataCtor (varId dv)
      mapM_ doCheck ctors
      where
        doCheck :: Monad m => Var -> TypeCheckT m ()
        doCheck cv = do
          meta <- Env.forceLookupRefMeta (varId cv)
          imps <- Env.forceLookupImplicit (varId cv)
          c <- Env.forceLookupRef (varId cv)
          strictPositivityCheck (refMetaLoc meta) dv imps (termTy c)
    checkPositivity _ = return ()

tcProgram :: MonadIO m => FilePath -> Program -> TypeCheckT m [RefVar]
tcProgram _ ([], defs) = tcDefsToVars IntMap.empty defs
tcProgram stdImport ((lo, ina) : imps, defs) = do
  cf <- ask
  let cd = takeDirectory cf
  liftIO (setCurrentDirectory cd)
  ina' <- fmap FilePath.normalise (importFilePath stdImport ina)
  isf <- liftIO (doesFileExist ina')
  when (not isf) (err lo (Fatal $ "unable to find import file \"" ++ ina ++ "\""))
  perms <- liftIO (getPermissions ina')
  when (not (readable perms)) (err lo (Fatal $ "unable to read file \"" ++ ina ++ "\""))
  is <- lift get
  if Set.member ina' is
    then tcProgram stdImport (imps, defs)
    else do
      lift (put (Set.insert ina' is))
      prog <- runParse ina'
      case prog of
        Left e -> throwError (Fatal e)
        Right prog' -> do
          vs <- local (const ina') (tcProgram stdImport prog')
          fmap (++vs) (tcProgram stdImport (imps, defs))

tcDefsToVars :: Monad m => SubstMap -> [Def] -> TypeCheckT m [RefVar]
tcDefsToVars subst defs = do
  ts <- tcDefs subst defs
  mapM extractDefVar ts
  where
    extractDefVar :: Monad m => Term -> TypeCheckT m RefVar
    extractDefVar t@(Term {termPre = (TermRef v _)}) = do
      rm <- Env.getRefMap
      if IntMap.member (varId v) rm
      then return (RefVal v)
      else do
        case preTermNormalize rm (termTy t) of
          TermArrow _ d _ -> return (RefExtern v (length d))
          _ -> error "expected nonexisting ref to be extern with function type"
    extractDefVar (Term {termPre = (TermData v)}) = return (RefData v)
    extractDefVar (Term {termPre = (TermCtor v _)}) = return (RefCtor v)
    extractDefVar t = do
      im <- Env.getImplicitMap
      rm <- Env.getRefMap
      error $ "unexpected def term: "
              ++ preTermToString im rm 0 (termPre t)

tcDefs :: Monad m => SubstMap -> [Def] -> TypeCheckT m [Term]
tcDefs subst defs =
  mapM_ (insertUnknownRef subst) defs >> mapM (tcDef subst True) defs

typeOperandName :: Monad m => PreTerm -> TypeCheckT m (Maybe VarName)
typeOperandName aty = do
  rm <- Env.getRefMap
  let aty' = preTermNormalize rm aty
  case aty' of
    TermArrow _ (_:_) _ -> return (Just "_->_")
    _ ->
      case preTermCodRootType rm aty' of
        Just (TermData v, _) -> return (Just (varName v))
        Just (TermTy, _) -> return (Just "Ty")
        _ -> return Nothing

tcDef :: Monad m => SubstMap -> Bool -> Def -> TypeCheckT m Term
tcDef subst _ (DefExtern d) = do
  depth <- Env.getDepth
  when (depth > 0)
    (err (declLoc d)
      (Fatal $ "nested " ++ quote "extern" ++ " in "
               ++ quote "where" ++ " clause is not supported"))
  ty1 <- Env.scope (tcDecl subst False d)
  case ty1 of
    Left term -> return term
    Right (imps, ty) -> do
      when (not (null imps))
        (err (declLoc d)
          (Fatal $ "unexpected implicit arguments on "
                   ++ quote "extern" ++ " value"))
      rm <- Env.getRefMap
      case preTermNormalize rm (termPre ty) of
        TermArrow _ _ _ -> return ()
        _ ->
          err (declLoc d)
            (Fatal $ "expected " ++ quote "extern"
                     ++ " to have function type "
                     ++ quote "->" ++ " or " ++ quote "->>")
      i <- Env.freshRefId
      let rv = mkVar i (declName d)
      Env.forceInsertImplicit i []
      let r = mkTerm (TermRef rv IntMap.empty) (termPre ty) False
      updateToStatusTerm (declName d) r
      Env.forceInsertExtern i
      return r
tcDef subst _ (DefVal isPure d lets) = do
  ty1 <- Env.scope (tcDecl subst False d)
  case ty1 of
    Left term -> return term
    Right (imps, ty) -> do
      i <- Env.freshRefId
      let rv = mkVar i (declName d)
      Env.forceInsertImplicit i imps
      let r = mkTerm (TermRef rv IntMap.empty) (termPre ty) False
      updateToStatusTerm (declName d) r
      e <- tcLetCases subst (declName d) imps (termPre ty) lets
      Env.forceInsertRef (declLoc d) (declName d) isPure i e
      terminationCheck (declLoc d) rv
      return r
tcDef subst True (DefData isPure d ctors) = do
  depth <- Env.getDepth
  when (depth > 0)
    (err (declLoc d)
      (Fatal $ "nested " ++ quote "data" ++ " type in "
               ++ quote "where" ++ " clause is not supported"))
  tcDataAndCtors subst isPure d ctors
tcDef subst False (DefData isPure d _) = do
  depth <- Env.getDepth
  when (depth > 0)
    (err (declLoc d)
      (Fatal $ "nested " ++ quote "data" ++ " type in "
               ++ quote "where" ++ " clause is not supported"))
  ty1 <- Env.scope (tcDecl subst False d)
  case ty1 of
    Left dt -> return dt
    Right (imps, ty) -> do
      i <- Env.freshRefId
      let v = mkVar i (declName d)
      let r = mkTerm (TermData v) (termPre ty) False
      updateToStatusTerm (declName d) r
      Env.forceInsertImplicit i imps
      Env.forceInsertRef (declLoc d) (declName d) isPure i r
      Env.forceInsertDataCtor i []
      return r

tcDataAndCtors :: Monad m =>
  SubstMap -> Bool -> Decl -> [Decl] -> TypeCheckT m Term
tcDataAndCtors subst isPure d ctors = do
  ty1 <- Env.scope (tcDecl subst False d)
  rm <- Env.getRefMap
  case ty1 of
    Left dt ->
      case termPre dt of
        TermData v -> do
          cis0 <- Env.forceLookupDataCtor (varId v)
          let css = Set.fromList (map varName cis0)
          cis1 <- foldrM (addVarNotIn css) [] ctors
          let cis' = cis0 ++ cis1
          let cis = map (takeCtorFormVarList cis') ctors
          Env.forceInsertDataCtor (varId v) cis
          updateToStatusTerm (varName v) dt
          mapM_ (tcDataDefCtor subst v) (zip cis ctors)
          Env.forceInsertRef (declLoc d) (declName d) isPure (varId v) dt
          return dt
        _ -> do
          im <- Env.getImplicitMap
          error $ "expected " ++ preTermToString im rm 0 (termPre dt)
                  ++ " to be a data type"
    Right (imps, ty) -> do
      let (fcod, hasIo) = preTermFinalCod rm (termPre ty)
      when hasIo
        (err (exprLoc (declType d))
          (Fatal "data type cannot have effectful function/lazy type"))
      when (not $ preTermsEqual rm TermTy fcod)
        (err (exprLoc (declType d))
          (Recoverable $
              "invalid type, the codomain root type needs to be " ++ quote "Ty"))
      i <- Env.freshRefId
      let v = mkVar i (declName d)
      cis <- mapM (\x ->
                    Env.freshRefId >>= \j ->
                      return $ mkVar j (declName x)) ctors
      let r = mkTerm (TermData v) (termPre ty) False
      Env.forceInsertDataCtor (varId v) cis
      updateToStatusTerm (declName d) r
      Env.forceInsertImplicit i imps
      Env.forceInsertRef (declLoc d) (declName d) isPure i r
      mapM_ (tcDataDefCtor subst v) (zip cis ctors)
      return r
  where
    addVarNotIn :: Monad m => Set VarName -> Decl -> [Var] -> TypeCheckT m [Var]
    addVarNotIn s x vs
      | Set.member (declName x) s =
          return vs
      | True = do
          j <- Env.freshRefId
          return (mkVar j (declName x) : vs)

    takeCtorFormVarList :: [Var] -> Decl -> Var
    takeCtorFormVarList [] c =
      error ("unable to find ctor " ++ declName c ++ " in var list")
    takeCtorFormVarList (v : vs) c
      | varName v == declName c = v
      | True = takeCtorFormVarList vs c

tcDataDefCtor :: Monad m =>
  SubstMap -> Var -> (Var, Decl) -> TypeCheckT m Term
tcDataDefCtor subst v (cva, ctor) = do
  ty1 <- Env.scope (tcDecl subst True ctor)
  case ty1 of
    Left c -> return c
    Right (imps, ty) -> do
      rm <- Env.getRefMap
      case preTermCodRootType rm (termPre ty) of
        Just (TermData (Var {varId = i}), hasIo) ->
          if i /= varId v
          then badCtorType
          else if hasIo
            then hasIoErr
            else return ()
        _ -> badCtorType
      let r = mkTerm (TermCtor cva (varId v)) (termPre ty) False
      updateToStatusTerm (declName ctor) r
      Env.forceInsertImplicit (varId cva) imps
      Env.forceInsertRef (declLoc ctor) (declName ctor) True (varId cva) r
      return r
  where
    badCtorType =
      err (declLoc ctor) (Recoverable $
        "constructor needs to have " ++
        "codomain root type given by data " ++ quote (varName v))
    hasIoErr =
      err (declLoc ctor)
        (Fatal "constructor cannot have effectful function/lazy type")

verifyNoIoEscapeFromType :: Monad m => Loc -> VarName -> Term -> TypeCheckT m ()
verifyNoIoEscapeFromType lo na t =
  when (termIo t) (err lo (Fatal $ "effect is escaping from type of " ++ quote na))

verifyNoUnfinishedRefs :: Monad m => Loc -> VarName -> PreTerm -> TypeCheckT m ()
verifyNoUnfinishedRefs lo na t = do
  rm <- Env.getRefMap
  rs <- filterM (fmap not . Env.isExtern) (IntSet.toList (preTermRefs rm t))
  mapM_ (verifyRefIsKnown rm) rs
  where
    verifyRefIsKnown :: Monad m => RefMap -> VarId -> TypeCheckT m ()
    verifyRefIsKnown rm i =
      when (not (IntMap.member i rm))
        (err lo (Fatal $ "constructor " ++ quote na ++ " has cyclic type"))

tcDecl :: Monad m =>
  SubstMap -> Bool -> Decl -> TypeCheckT m (Either Term (Implicits, Term))
tcDecl subst isCtor d = do
  doTcDecl subst isCtor d `catchError`
    (\e -> case e of
            Recoverable m -> throwError (Fatal m)
            Fatal m -> throwError (Fatal m))

doTcDecl :: Monad m =>
  SubstMap -> Bool -> Decl -> TypeCheckT m (Either Term (Implicits, Term))
doTcDecl subst isCtor (Decl (lo, na) impls ty) = do
  let (na0, opty) = operandSplit na
  checkOperandTypeString (lo, na0) opty
  t <- markInProgress isCtor (lo, na)
  case t of
    Nothing -> do
      mapM_ insertUnknownImplicit impls
      impls' <- mapM checkImplicit impls
      ty' <- evalTcExpr subst TermTy ty
      verifyNoIoEscapeFromType lo na ty'
      verifyNoUnfinishedRefs lo na (termPre ty')
      verifyOperandArgument (termPre ty')
      return (Right (impls', ty'))
    Just t' ->
      return (Left t')
  where
    verifyOperandArgument :: Monad m => PreTerm -> TypeCheckT m ()
    verifyOperandArgument dty
      | isRightAssocInfixOp na = do
          rm <- Env.getRefMap
          case preTermDomCod rm dty of
            Nothing -> errorInfixArgs
            Just ([_, (_, a)], _, _) -> verifyOperandType a
            Just _ -> errorInfixArgs
      | isLeftAssocInfixOp na = do
          rm <- Env.getRefMap
          case preTermDomCod rm dty of
            Nothing -> errorInfixArgs
            Just ([(_, a), _], _, _) -> verifyOperandType a
            Just _ -> errorInfixArgs
      | isPrefixOp na = do
          rm <- Env.getRefMap
          case preTermDomCod rm dty of
            Nothing -> errorPrefixArgs
            Just ([(_, a)], _, _) -> verifyOperandType a
            Just _ -> errorPrefixArgs
      | isPostfixOp na = do
          rm <- Env.getRefMap
          case preTermDomCod rm dty of
            Nothing -> errorPostfixArgs
            Just ([], _, _) -> errorPostfixArgs
            Just ((_, a) : _, _, _) -> verifyOperandType a
      | True = return ()

    errorInfixArgs :: Monad m => TypeCheckT m ()
    errorInfixArgs =
      err lo (Fatal $ "expected infix operator to have exactly two function arguments")

    errorPrefixArgs :: Monad m => TypeCheckT m ()
    errorPrefixArgs =
      err lo (Fatal $ "expected prefix operator to have exactly one function argument")

    errorPostfixArgs :: Monad m => TypeCheckT m ()
    errorPostfixArgs =
      err lo (Fatal $ "expected postfix operator to have at least one function argument")

    verifyOperandType :: Monad m => PreTerm -> TypeCheckT m ()
    verifyOperandType _ = return ()

    insertUnknownImplicit :: Monad m => VarListElem -> TypeCheckT m ()
    insertUnknownImplicit (_, Nothing) =
      error "expected implicit argument to have a type spec (@1)"
    insertUnknownImplicit ((vlo, vna), Just e) = do
      checkVarNameValid (vlo, vna)
      insertUnknownVar subst (vlo, vna) (Just e)

    checkImplicit :: Monad m =>
      VarListElem -> TypeCheckT m (Var, PreTerm)
    checkImplicit (_, Nothing) =
      error "expected implicit argument to have a type spec (@2)"
    checkImplicit ((vlo, vna), Just e)
      | vna == "_" =
          err vlo (Fatal $ "invalid implicit name " ++ quote "_")
      | True = do
          pa <- markInProgress False (vlo, vna)
          case pa of
            Nothing -> do
              e' <- evalTcExpr subst TermTy e
              verifyNoIoEscapeFromType vlo vna e'
              i <- Env.freshVarId
              let v = mkVar i vna
              updateToStatusTerm vna (mkTerm (TermVar False v) (termPre e') False)
              return (v, termPre e')
            Just (Term {termPre = TermVar _ v, termTy = tt}) ->
              return (v, tt)
            _ ->
              error $ "not a variable term " ++ quote vna

checkOperandTypeString :: Monad m => (Loc, VarName) -> Maybe VarName -> TypeCheckT m ()
checkOperandTypeString (nlo, na) Nothing =
  when (isOperator na)
    (err nlo (Fatal $ "operator declaration is missing operand type argument "
                      ++ quote "\\..."))
checkOperandTypeString (nlo, na) (Just ss) = do
  when (not (isOperator na))
    (err nlo (Fatal $ "operand type argument " ++ quote "\\..."
                      ++ " is valid only for operators"))
  when (not (null ss) && not (isKeyOperandType ss)) $ do
    st <- Env.lookup ss
    case st of
      Nothing -> err nlo (Fatal $ "unexpected operand type " ++ quote ss) 
      Just st' -> do
        vt <- varStatusTerm st'
        case termPre vt of
          TermData _ -> return ()
          _ -> err nlo (Fatal $ "unexpected operand type " ++ quote ss
                                ++ ", not a data type")
  where
    isKeyOperandType :: String -> Bool
    isKeyOperandType "Ty" = True
    isKeyOperandType "_->_" = True
    isKeyOperandType _ = False

tcVar :: Monad m => SubstMap -> (Loc, VarName) -> Expr -> TypeCheckT m Term
tcVar subst (lo, na) e = do
  t <- markInProgress False (lo, na)
  case t of
    Nothing -> do
      ty <- evalTcExpr subst TermTy e
      verifyNoIoEscapeFromType lo na ty
      i <- Env.freshVarId
      let v = mkTerm (TermVar False (mkVar i na)) (termPre ty) False
      updateToStatusTerm na v
      return v
    Just t' -> return t'

tcDataDefCtorLookup :: Monad m =>
  SubstMap -> (Loc, VarName) -> Decl -> TypeCheckT m Term
tcDataDefCtorLookup subst (dlo, dna) d = do
  x <- Env.lookup dna
  dt <- case x of
          Nothing ->
            error ("cannot find data type " ++ quote dna
                   ++ " for ctor " ++ quote (declName d))
          Just (StatusUnknownCtor _ _ _ _) ->
            error ("ctor " ++ quote dna
                   ++ " was expected to be data type")
          Just (StatusUnknownVar _ _ _ _) ->
            error ("variable " ++ quote dna
                   ++ " was expected to be data type")
          Just (StatusTerm t) ->
            return t
          Just x'@(StatusUnknownRef _ _ _) ->
            varStatusTerm x'
          Just (StatusInProgress _ _) ->
            err dlo (Fatal $ "data type " ++ quote dna ++ " has cyclic type")
  case termPre dt of
    TermData v -> do
      dis <- Env.forceLookupDataCtor (varId v)
      ctor <- Env.lookup (declName d)
      case ctor of
        Just (StatusTerm ctor') ->
          return ctor'
        Just (StatusInProgress _ _) ->
          err dlo (Fatal $
                    "constructor " ++ quote (declName d)
                    ++ " has cyclic type")
        Just (StatusUnknownCtor _ _ _ _) -> do
          eis <- Env.forceLookupDataCtor (varId v)
          case find (\ei -> varName ei == declName d) eis of
            Nothing -> do
              i <- Env.freshRefId
              let cv = mkVar i (declName d)
              Env.forceInsertDataCtor (varId v) (cv : dis)
              tcDataDefCtor subst v (cv, d)
            Just cv ->
              tcDataDefCtor subst v (cv, d)
        Nothing ->
          error ("data type in scope " ++ quote dna
                 ++ ", but ctor " ++ declName d ++ " is not")
        _ -> 
          error ("just type checked data type " ++ quote dna
                 ++ ", but ctor " ++ declName d
                 ++ " is not in env state StatusTerm")
    _ ->
      error $ "expected " ++ dna ++ " to be data type"

tcLetCases :: Monad m =>
  SubstMap -> VarName -> Implicits -> PreTerm -> [LetCase] -> TypeCheckT m Term
tcLetCases subst na [] ty [LetCase lo' [] Nothing (Just (e, w))] = do
  case w of
    Nothing -> do
      e' <- evalTcExpr subst ty e
      when (termIo e') (err lo' (Fatal $ "effect escapes from " ++ quote na))
      return e'
    Just w' -> Env.scope $ do
      vs <- tcDefsToVars subst w'
      e' <- evalTcExpr subst ty e
      when (termIo e') (err lo' (Fatal $ "effect escapes from " ++ quote na))
      return (e' {termNestedDefs = vs})
tcLetCases _ na [] _ (LetCase lo' [] Nothing Nothing : _) = do
  err lo' (Fatal $ "case for " ++ quote na ++ " is not absurd")
tcLetCases _ _ [] _ (LetCase _ [] Nothing (Just _) : LetCase lo' _ _ _ : _) = do
  err lo' (Fatal "case is unreachable")
tcLetCases _ na [] _ (LetCase lo' (_:_) Nothing _ : _) = do
  err lo' (Fatal $ "cannot apply " ++ quote na ++ " to implicit arguments")
tcLetCases subst na ips@(_:_) ty (le@(LetCase lo' _ Nothing _) : lets) = do
  (ct, io) <- buildLetCaseTree na subst ips Nothing ty (le : lets)
  when io (err lo' (Fatal $ "effect escapes from " ++ quote na))
  let ins = map (varName . fst) ips
  let fn = TermFun ins False Nothing ct
  return (mkTerm fn ty False)
tcLetCases subst na ips ty (le@(LetCase lo' _ (Just _) _) : lets) = do
  r <- Env.getRefMap
  case preTermDomCod r ty of
    Nothing -> err lo' (Recoverable $
                          "application of " ++ quote na
                          ++ ", but does not have function type")
    Just (dom, cod, io) -> do
      (ct, hasIo) <- buildLetCaseTree na subst ips (Just dom) cod (le : lets)
      let ins = map (varName . fst) ips
      when (hasIo && not io)
        (err lo' (
          Recoverable $
            "expected type of " ++ quote na
            ++ " to be effectful " ++ quote "->>"))
      return (mkTerm (TermFun ins hasIo (Just (length dom)) ct) ty False)
tcLetCases _ na _ _ [] = error ("missing let case for " ++ quote na)

type CasePatternTriple =
  ((Maybe Var, Bool), Pattern) -- Bool = True if forced.

buildLetCaseTree :: Monad m =>
  VarName -> SubstMap -> Implicits -> Maybe [(Maybe Var, PreTerm)] -> PreTerm ->
  [LetCase] -> TypeCheckT m (CaseTree, Bool)
buildLetCaseTree valName subst impParams domain codomain letCases = do
  iCaseData <- extractImplicitCaseData letCases
  r0 <- Env.getRefMap
  let iCaseData' = dependencyOrder r0 iCaseData
                    (map (\(v,x) -> (Just v, x)) impParams)
  caseData <- case domain of
                Nothing ->
                  verifyNoExplicitArgs letCases >> return []
                Just d -> do
                  cd <- extractExplicitCaseData (length d) letCases
                  r1 <- Env.getRefMap
                  return (dependencyOrder r1 cd d)
  let caseData' = if null caseData
                    then iCaseData'
                    else mergeCaseData (length impParams) iCaseData' caseData
  cs <- mapM checkCase caseData'
  casesToCaseTree cs
  where
    verifyNoExplicitArgs :: Monad m => [LetCase] -> TypeCheckT m ()
    verifyNoExplicitArgs [] = return ()
    verifyNoExplicitArgs (LetCase lo _ (Just _) _ : _) =
      err lo (Recoverable $ "unexpected function application of " ++ quote valName)
    verifyNoExplicitArgs (LetCase _ _ Nothing _ : ls) =
      verifyNoExplicitArgs ls

    extractImplicitCaseData :: Monad m => [LetCase] ->
      TypeCheckT m [([ParsePattern], Loc, Maybe (Expr, OptWhereClause))]
    extractImplicitCaseData [] = return []
    extractImplicitCaseData (LetCase lo ias _ e : ls) = do
      let (p, remain) = getImplicitPatterns lo impParams ias
      case remain of
        [] -> do
          ps <- extractImplicitCaseData ls
          return ((p, lo, e) : ps)
        ((_, n'), _) : _ -> 
          if isJust (find isInImpParams remain)
            then err lo (Fatal $
                          "implicit argument "
                          ++ quote n' ++ " is matched twice")
            else err lo (Fatal $
                          "unexpected implicit "
                          ++ quote n' ++ " for " ++ quote valName)
      where
        isInImpParams :: ((Loc, String), ParsePattern) -> Bool
        isInImpParams ((_, n'), _) =
          isJust $ find (\(v, _) -> varName v == n') impParams

    getImplicitPatterns ::
      Loc -> Implicits -> [((Loc, String), ParsePattern)] ->
      ([ParsePattern], [((Loc, String), ParsePattern)])
    getImplicitPatterns _ [] ps = ([], ps)
    getImplicitPatterns lo ((n, _) : is) ps =
      let (q, ps') = getImplicitPattern lo (varName n) ps
          (qs, ps0) = getImplicitPatterns lo is ps'
      in (q : qs, ps0)

    getImplicitPattern ::
      Loc -> VarName -> [((Loc, String), ParsePattern)] ->
      (ParsePattern, [((Loc, String), ParsePattern)])
    getImplicitPattern lo n isa =
      case findPat isa of
        Nothing -> (ParsePatternVar (lo, n), isa)
        Just x -> x
      where
        findPat ::
          [((Loc, String), ParsePattern)] ->
          Maybe (ParsePattern, [((Loc, String), ParsePattern)])
        findPat [] = Nothing
        findPat (q@((_, n'), p) : ps) =
          if n == n'
            then Just (p, ps)
            else case findPat ps of
                  Nothing -> Nothing
                  Just (p', ps') -> Just (p', q : ps')

    extractExplicitCaseData :: Monad m => Int -> [LetCase] ->
      TypeCheckT m [([ParsePattern], Loc, Maybe (Expr, OptWhereClause))]
    extractExplicitCaseData _ [] = return []
    extractExplicitCaseData n (LetCase lo _ (Just ps) e : ls) = do
      when (length ps /= n)
        (err lo (Fatal $
          "expected " ++ show n ++ " argument(s), but case for "
          ++ quote valName ++ " is applied to " ++ show (length ps) ++ " argument(s)"))
      pss <- extractExplicitCaseData n ls
      return ((ps, lo, e) : pss)
    extractExplicitCaseData n (LetCase lo _ Nothing _ : _) =
      err lo (Fatal $
        "expected " ++ show n ++ " argument(s) in case for " ++ quote valName)

    mergeCaseData ::
      Int ->
      [([(Int, (Maybe Var, PreTerm), ParsePattern)],
            Loc, Maybe (Expr, OptWhereClause))] ->
      [([(Int, (Maybe Var, PreTerm), ParsePattern)],
            Loc, Maybe (Expr, OptWhereClause))] ->
      [([(Int, (Maybe Var, PreTerm), ParsePattern)],
            Loc, Maybe (Expr, OptWhereClause))]
    mergeCaseData _ [] [] = []
    mergeCaseData n ((ip, lo, e) : is) ((pp, _, _) : xs) =
      (ip ++ incrImplicits pp, lo, e) : mergeCaseData n is xs
      where
        incrImplicits ::
          [(Int, (Maybe Var, PreTerm), ParsePattern)] ->
          [(Int, (Maybe Var, PreTerm), ParsePattern)]
        incrImplicits [] = []
        incrImplicits ((i, v, p) : ps) =
          (i + n, v, p) : incrImplicits ps
    mergeCaseData _ _ _ =
      error "different number of implicit and explicit cases"

    dependencyOrder ::
      RefMap -> [([ParsePattern], Loc, Maybe (Expr, OptWhereClause))] ->
      [(Maybe Var, PreTerm)] ->
      [([(Int, (Maybe Var, PreTerm), ParsePattern)],
            Loc, Maybe (Expr, OptWhereClause))]
    dependencyOrder _ [] _ = []
    dependencyOrder r ((ps, lo, e) : xs) d =
      (dependencyOrderArgs (preTermVars r) d ps, lo, e) : dependencyOrder r xs d

    checkCase :: Monad m =>
      ([(Int, (Maybe Var, PreTerm), ParsePattern)], Loc, Maybe (Expr, OptWhereClause)) ->
      TypeCheckT m (Loc, [CasePatternTriple], Maybe Term)
    checkCase (args, lo, e) = doCheckCase args lo e

    doCheckCase :: Monad m =>
      [(Int, (Maybe Var, PreTerm), ParsePattern)] -> Loc ->
      Maybe (Expr, OptWhereClause) ->
      TypeCheckT m (Loc, [CasePatternTriple], Maybe Term)
    doCheckCase args lo (Just (e, w)) =
      Env.scope $ do
        (tsu, bsu, ps) <- checkArgs args
        vs <- case w of
                Just w' -> tcDefsToVars bsu w'
                _ -> return []
        b <- evalTcExpr bsu (substPreTerm bsu $ substPreTerm tsu codomain) e
        return (lo, patternsToTriple ps, Just (b {termNestedDefs = vs}))
    doCheckCase args lo Nothing =
      Env.scope $ do
        (_, _, ps) <- checkArgs args
        return (lo, patternsToTriple ps, Nothing)

    patternsToTriple :: [(Maybe Var, Pattern)] -> [CasePatternTriple]
    patternsToTriple [] = []
    patternsToTriple ((v, p) : ps) =
      ((v, False), p) : patternsToTriple ps

    checkArgs :: Monad m =>
      [(Int, (Maybe Var, PreTerm), ParsePattern)] ->
      TypeCheckT m (SubstMap, SubstMap, [(Maybe Var, Pattern)])
    checkArgs as = do
      newpids <- Env.getNextVarId
      (m1, m2, ps) <- tcPatternArgs newpids IntMap.empty subst as
      return (m1, m2, map snd (sortOn fst ps))

{-
showCasesToCaseTree ::
  ImplicitMap -> RefMap -> [(Loc, [CasePatternTriple], Maybe Term)] -> String
showCasesToCaseTree _ _ [] = ""
showCasesToCaseTree im rm ((_, ps, _) : pss) =
  let ss = map varPatToStr ps
  in intercalate ", " ss ++ "\n" ++ showCasesToCaseTree im rm pss
  where
    varPatToStr ((Nothing, b), p) =
      prePatternToString im rm 0 (patternPre p)
      ++ " : " ++ preTermToString im rm 0 (patternTy p)
      ++ " {forced? " ++ show b ++ "}"
    varPatToStr ((Just v, b), p) =
      varName v ++ "." ++ show (varId v) ++ " := " ++ varPatToStr ((Nothing, b), p)
-}

-- Bool = True if there is a leaf which is effectful.
casesToCaseTree :: Monad m =>
  [(Loc, [CasePatternTriple], Maybe Term)] -> TypeCheckT m (CaseTree, Bool)
casesToCaseTree pss = do
  x <- runExceptT (doCasesToCaseTree 0 pss)
  case x of
    Left lo -> err lo (Fatal "non-exhaustive patterns")
    Right (t, reachedCases) -> do
      let allCases = projectLocs pss
      let diff = Set.difference allCases reachedCases
      case Set.elems diff of
        [] -> return (t, caseTreeHasIo t)
        c:_ -> err c (Fatal "case is unreachable")
  where
    projectLocs :: [(Loc, a, b)] -> Set Loc
    projectLocs [] = Set.empty
    projectLocs ((lo, _, _) : los) =
      Set.insert lo (projectLocs los)

doCasesToCaseTree :: Monad m =>
  Int -> [(Loc, [CasePatternTriple], Maybe Term)] ->
  ExceptT Loc (TypeCheckT m) (CaseTree, Set Loc)
doCasesToCaseTree _ [] = error "no cases to build case tree"
doCasesToCaseTree startIdx pss@((firstLoc, ps, te) : _) = do
  --rr <- lift Env.getRefMap
  --ii <- lift Env.getImplicitMap
  --let !() = trace (showCasesToCaseTree ii rr pss) ()
  midx <- lift (findMatchIndex ps)
  case midx of
    Left False -> throwError firstLoc
    Left True -> do
      let (emp, vis) = foldr (\(i, (_, p)) (b, vs) ->
                        if isJust b
                        then (b, vs)
                        else
                          case prePatternGetRoot (patternPre p) of
                            PatternVar v -> (b, v : vs)
                            PatternEmpty -> (Just i, [])
                            _ -> error "expected pattern var or empty"
                          ) (Nothing, []) (zip [0..] ps)
      case emp of
        Just idx -> do
          when (isJust te)
            (lift $ err firstLoc (Fatal "unexpected case, it is absurd"))
          return (CaseEmpty idx, Set.singleton firstLoc)
        Nothing ->
          case te of
            Nothing -> lift . err firstLoc $ Fatal "missing case(s)"
            Just te' -> do
              let t = CaseLeaf vis (termIo te')
                          (termPre te') (termNestedDefs te')
              return (t, Set.singleton firstLoc)
    Right (idx, dty) -> do
      --let !() = trace ("pattern match index: " ++ show idx) ()
      dsp <- lift (dataSplit dty)
      --let !() = trace ("data split: " ++ show (IntMap.keysSet dsp)) ()
      let cids = IntSet.toList (getCtorVarIds idx pss)
      pss' <- lift $ duplicateCatchAll idx cids pss
      csp <- lift $ splitCtorCases idx pss'
      --let !() = trace ("split cases: " ++ show (IntMap.keysSet csp)) ()
      let missing = IntSet.difference (IntMap.keysSet dsp) (IntMap.keysSet csp)
      allCtorsCovered <- lift $ testAllCtorsCovered idx (ps !! idx) missing
      --let !() = trace ("all ctors covered? " ++ show allCtorsCovered) ()
      ca <- if allCtorsCovered
              then return Nothing
              else lift $ makeCatchAllCases idx pss'
      if isNothing ca && not allCtorsCovered
        then throwError firstLoc
        else do
          subc0 <- mapM makeSubCase csp
          let (subc, subcLoc) = IntMap.foldlWithKey
                                          mergeLocs
                                          (IntMap.empty, Set.empty)
                                          subc0
          (subd, subdLoc) <- case ca of
                              Nothing -> return (Nothing, Set.empty)
                              Just ca' -> do
                                (d, lo) <- makeSubCase ca'
                                return (Just d, lo)
          cnode <- lift $ makeCaseNode idx subc (ca, subd)
          return (cnode, Set.union subcLoc subdLoc)
  where
    mergeLocs ::
      (IntMap ([Var], CaseTree), Set Loc) ->
      Int -> (([Var], CaseTree), Set Loc) ->
      (IntMap ([Var], CaseTree), Set Loc)
    mergeLocs (m, s) i (b, t) = (IntMap.insert i b m, Set.union s t)

    makeSubCase :: Monad m =>
      ([(Loc, [CasePatternTriple], Maybe Term)], [Var]) ->
      ExceptT Loc (TypeCheckT m) (([Var], CaseTree), Set Loc)
    makeSubCase (rss, ids) = do
      (tr, lo) <- doCasesToCaseTree 0 rss
      return ((ids, tr), lo)

    makeCaseNode :: Monad m =>
      Int -> IntMap ([Var], CaseTree) ->
        (Maybe ([(Loc, [CasePatternTriple], Maybe Term)], [Var]),
         Maybe ([Var], CaseTree)) ->
      TypeCheckT m CaseTree
    makeCaseNode idx subc (_, subd) =
      case IntMap.lookup (-1) subc of
        Nothing -> return (CaseNode idx subc subd)
        Just subc' -> return (CaseUnit idx subc')

    findMatchIndex :: Monad m =>
      [CasePatternTriple] -> TypeCheckT m (Either Bool (Int, VarId))
    findMatchIndex patterns = doFindMatchIndex startIdx (drop startIdx patterns)
      where
        doFindMatchIndex :: Monad m =>
          Int -> [CasePatternTriple] -> TypeCheckT m (Either Bool (Int, VarId))
        doFindMatchIndex _ [] =
          if startIdx == 0 then return (Left True) else return (Left False)
        doFindMatchIndex idx ((_, q) : qs) =
          case prePatternGetRoot (patternPre q) of
            PatternUnit -> return (Right (idx, -1))
            PatternCtor _ i -> do
              d <- hasLateDependency q qs
              if not d
              then return (Right (idx, i))
              else doFindMatchIndex (idx + 1) qs
            _ -> doFindMatchIndex (idx + 1) qs

        hasLateDependency :: Monad m =>
          Pattern -> [CasePatternTriple] -> TypeCheckT m Bool
        hasLateDependency q qs = do
          rm <- Env.getRefMap
          let fs = preTermVars rm (patternTy q)
          if IntSet.null fs
          then return False
          else return (doHasLateDependency fs qs)

        doHasLateDependency :: IntSet -> [CasePatternTriple] -> Bool
        doHasLateDependency _ [] = False
        doHasLateDependency vs (((Nothing, _), _) : qs) =
          doHasLateDependency vs qs
        doHasLateDependency vs (((Just v, _), q) : qs) =
          case prePatternGetRoot (patternPre q) of
            PatternEmpty -> doHasLateDependency vs qs
            PatternVar _ -> doHasLateDependency vs qs
            _ -> IntSet.member (varId v) vs || doHasLateDependency vs qs

    -- Returns map VarId -> PreTerm, where VarId is ctor id,
    -- PreTerm is the ctor's type, and VarId = -1 means unit.
    dataSplit :: Monad m =>
      VarId -> TypeCheckT m (IntMap PreTerm)
    dataSplit (-1) = return (IntMap.singleton (-1) TermUnitTy)
    dataSplit dv = do
      is <- Env.forceLookupDataCtor dv
      foldlM insm IntMap.empty is
      where
        insm :: Monad m => IntMap PreTerm -> Var -> TypeCheckT m (IntMap PreTerm)
        insm m i = do
          r <- Env.forceLookupRef (varId i)
          return (IntMap.insert (varId i) (termTy r) m)

    -- VarId = -1 is unit.
    getCtorVarIds ::
      Int -> [(Loc, [CasePatternTriple], Maybe Term)] -> IntSet
    getCtorVarIds _ [] = IntSet.empty
    getCtorVarIds idx (((_, qs, _)) : qss) = 
      let (_, q) = qs !! idx
          i = case prePatternGetRoot (patternPre q) of
               PatternCtor v _ -> Just (varId v)
               PatternUnit -> Just (-1)
               _ -> Nothing
      in case i of
          Just i' -> IntSet.insert i' (getCtorVarIds idx qss)
          Nothing -> getCtorVarIds idx qss

    duplicateCatchAll :: Monad m =>
      Int -> [VarId] -> [(Loc, [CasePatternTriple], Maybe Term)] ->
      TypeCheckT m [(Loc, [CasePatternTriple], Maybe Term, [Var])]
    duplicateCatchAll _ _ [] = return []
    duplicateCatchAll idx cids ((lo, qs, t) : rss) = do
      let (qh, qt1) = splitAt idx qs
      let ([(b, q)], qt) = splitAt 1 qt1
      case prePatternGetRoot (patternPre q) of
        PatternVar pvar -> do
          d <- makeCtorPatterns b cids (patternTy q)
          let d' = map (\x -> (lo, qh ++ [x] ++ qt, t, [pvar])) d
          e <- duplicateCatchAll idx cids rss
          return ((lo, qs, t, []) : d' ++ e)
        PatternEmpty -> do
          d <- makeCtorPatterns b cids (patternTy q)
          vi <- Env.freshVarId
          let d' = map (\x -> (lo, qh ++ [x] ++ qt, t, [mkVar vi "_"])) d
          e <- duplicateCatchAll idx cids rss
          return ((lo, qs, t, []) : d' ++ e)
        _ -> fmap ((lo, qs, t, []):) (duplicateCatchAll idx cids rss)

    testAllCtorsCovered :: Monad m =>
      Int -> CasePatternTriple -> IntSet -> TypeCheckT m Bool
    testAllCtorsCovered _ ((_, True), _) _ = return True
    testAllCtorsCovered idx ((_, False), p) s = do
      xs <- makeCtorPatterns (Nothing, False) (IntSet.toList s) (patternTy p)
      if null xs
      then return True
      else
        let idx' = idx + 1
        in if idx' < length ps
           then do
            let pss' = dropDefaultAndEmptyCases idx pss
            if null pss'
            then return True
            else do
              y <- runExceptT (doCasesToCaseTree idx' pss')
              case y of
                Left _ -> return False
                Right _ -> return True
           else return False

    dropDefaultAndEmptyCases ::
      Int -> [(Loc, [CasePatternTriple], Maybe Term)] ->
      [(Loc, [CasePatternTriple], Maybe Term)]
    dropDefaultAndEmptyCases _ [] = []
    dropDefaultAndEmptyCases idx (c@(_, qs, _) : qss) =
      let ((_, _), q) = qs !! idx
      in case prePatternGetRoot (patternPre q) of
          PatternVar _ -> dropDefaultAndEmptyCases idx qss
          PatternEmpty -> dropDefaultAndEmptyCases idx qss
          _ -> c : dropDefaultAndEmptyCases idx qss

    makeCtorPatterns :: Monad m =>
      (Maybe Var, Bool) -> [VarId] -> PreTerm -> TypeCheckT m [CasePatternTriple]
    makeCtorPatterns _ [] _ = return []
    makeCtorPatterns b (-1 : is) ty = do
      newpids <- Env.getNextVarId
      r <- runExceptT (patternUnify2 newpids ty TermUnitTy)
      case r of
        Left (UnifyAbsurd _) -> makeCtorPatterns b is ty
        _ -> do
          ps' <- makeCtorPatterns b is ty
          let c = Pattern {patternPre = PatternUnit,
                           patternTy = TermUnitTy}
          return ((b, c) : ps')
    makeCtorPatterns b (i : is) ty = do
      x <- Env.forceLookupRef i
      let cp = case termPre x of
                TermCtor vv ii -> PatternCtor vv ii
                _ -> error $ "expected ref to be a ctor " ++ show i
      c <- prePatternApplyWithVars cp (termTy x)
      newpids <- Env.getNextVarId
      --im0 <- Env.getImplicitMap
      --rm0 <- Env.getRefMap
      --let !() = trace ("new ctor pattern " ++ prePatternToString im0 rm0 0 (patternPre c)) ()
      --let !() = trace ("unify pattern type " ++ preTermToString im0 rm0 0 (patternTy c) ++ " with " ++ preTermToString im0 rm0 0 ty) ()
      r <- runExceptT (patternUnify2 newpids ty (patternTy c))
      case r of
        Left (UnifyAbsurd _) -> do
          makeCtorPatterns b is ty
        _ -> do
          ps' <- makeCtorPatterns b is ty
          return ((b, c {patternTy = ty}) : ps')

    splitCtorCases :: Monad m =>
      Int -> [(Loc, [CasePatternTriple], Maybe Term, [Var])] ->
      TypeCheckT m (IntMap ([(Loc, [CasePatternTriple], Maybe Term)], [Var]))
    splitCtorCases _ [] = return IntMap.empty
    splitCtorCases idx ((lo, qs, t, ids) : rss) = do
      m <- splitCtorCases idx rss
      let (((name, _), q), qs') = removeIdx idx qs
      let i = case prePatternGetRoot (patternPre q) of
               PatternCtor v cis -> Just (varId v, PatternCtor v cis)
               PatternUnit -> Just (-1, PatternUnit)
               _ -> Nothing
      case i of
        Nothing -> return m
        Just (i', c) -> do
          newpids <- Env.getNextVarId
          im <- Env.getImplicitMap
          rm <- Env.getRefMap
          let (is, as) = patternProjArgs im rm q
          is' <- pvarsImplicits is
          as' <- pvarsArgs as
          cte <- if i' == -1
                  then return mkTermUnitElem
                  else Env.forceLookupRef i'
          let is1p = map (\(n, v) -> (n, PatternVar v)) is'
          let as1p = map (fmap (map PatternVar)) as'
          let c' = patternApply im rm
                    (Pattern {patternPre = c, patternTy = termTy cte}) is1p as1p
          let (is1, as1) = patternProjArgs im rm c'
          let as2 = makeNewVars (Just (map snd is') : as') (Just (map snd is) : as) (Just (map snd is1) : as1)
          let tsu = case name of
                      Nothing -> IntMap.empty
                      Just n ->
                        let x = prePatternToPreTerm (patternPre c')
                        in IntMap.singleton (varId n) x
          --im0 <- Env.getImplicitMap
          --rm0 <- Env.getRefMap
          --let !() = trace ("unify " ++ preTermToString im0 rm0 0 (patternTy q) ++ " with " ++ preTermToString im0 rm0 0 (patternTy c')) ()
          un <- runExceptT (patternUnify2 newpids (patternTy c') (patternTy q))
          let msu = case un of
                      Left (UnifyAbsurd _) -> Nothing
                      Left _ -> Just IntMap.empty
                      Right su -> Just su
          case msu of
            Nothing -> return m
            Just bsu0 -> do
              let rs0 = as2 ++ qs'
              bsu <- reorderForceSubst newpids bsu0 rs0
              let rs1 = substTripleForce tsu bsu rs0
              let rs = (lo, substForcedCatchAllPatterns bsu rs1, t)
              --let !() = trace ("initial data:\n" ++ showCasesToCaseTree im0 rm0 [(lo, rs0, t)]) ()
              case IntMap.lookup i' m of
                Nothing -> do
                  --let !() = trace ("single case:\n" ++ showCasesToCaseTree im0 rm0 [rs]) ()
                  return $ IntMap.insert i' ([rs], ids) m
                Just (xs, ids') -> do
                  --let !() = trace ("add to cases yields:\n" ++ showCasesToCaseTree im0 rm0 (rs : xs)) ()
                  return $ IntMap.insert i' ((rs : xs), ids ++ ids') m

    reorderForceSubst :: Monad m =>
      VarId -> SubstMap -> [CasePatternTriple] -> TypeCheckT m SubstMap
    reorderForceSubst newpids su qs = do
      su' <- reorderForceSubstStep newpids su qs
      if IntMap.keys su /= IntMap.keys su'
      then reorderForceSubst newpids su' qs
      else return su'

    reorderForceSubstStep :: Monad m =>
      VarId -> SubstMap -> [CasePatternTriple] -> TypeCheckT m SubstMap
    reorderForceSubstStep newpids su qs =
      let (n1, n2) = IntMap.foldrWithKey (\i b (m1, m2) ->
                      case b of
                        TermVar _ b' ->
                          if needsReorder i (varId b') qs
                          then (IntMap.insert (varId b')
                                  (TermVar False (mkVar i "_")) m1, m2)
                          else (m1, IntMap.insert i b m2)
                        _ -> (m1, IntMap.insert i b m2)
                    ) (IntMap.empty, IntMap.empty) su
      in tcMergePatUnifMaps newpids firstLoc n1 n2
      where
        findLatestInTriple :: VarId -> VarId -> VarId -> [CasePatternTriple] -> VarId
        findLatestInTriple a _ _ [] = a
        findLatestInTriple a i j (((Nothing, _), _) : xs) =
          findLatestInTriple a i j xs
        findLatestInTriple a i j (((Just v, _), _) : xs) =
          if varId v == i
          then findLatestInTriple i i j xs
          else if varId v == j 
               then findLatestInTriple j i j xs
               else findLatestInTriple a i j xs

        needsReorder :: VarId -> VarId -> [CasePatternTriple] -> Bool
        needsReorder i j xs = findLatestInTriple i i j xs == j

    substMapToPatternMap :: SubstMap -> [CasePatternTriple] -> IntMap Pattern
    substMapToPatternMap _ [] = IntMap.empty
    substMapToPatternMap su (((Just v, True), p) : qs) =
      let psu = substMapToPatternMap su qs
      in case IntMap.lookup (varId v) su of
          Just (TermVar _ v') ->
            case patternPre p of
              PatternVar _ -> psu
              _ -> IntMap.insert (varId v') p psu
          Nothing -> psu
          Just _ -> psu
    substMapToPatternMap su (_ : qs) = substMapToPatternMap su qs

    substForcedCatchAllPatterns ::
      SubstMap -> [CasePatternTriple] -> [CasePatternTriple]
    substForcedCatchAllPatterns su qs =
      substForcedCatchAllPatterns' (substMapToPatternMap su qs) qs

    substForcedCatchAllPatterns' ::
      IntMap Pattern -> [CasePatternTriple] -> [CasePatternTriple]
    substForcedCatchAllPatterns' _ [] = []
    substForcedCatchAllPatterns' su
        (q@((Just v, False), (Pattern {patternPre = PatternVar _})) : qs) =
      case IntMap.lookup (varId v) su of
        Nothing -> q : substForcedCatchAllPatterns' su qs
        Just p -> ((Just v, False), p) : substForcedCatchAllPatterns' su qs
    substForcedCatchAllPatterns' su (q : qs) =
      q : substForcedCatchAllPatterns' su qs
    
    makeNewVars ::
      [Maybe [Var]] -> [Maybe [Pattern]] -> [Maybe [Pattern]] -> [CasePatternTriple]
    makeNewVars [] [] [] = []
    makeNewVars (Nothing : vss) (Nothing : qss) (Nothing : rss) =
      makeNewVars vss qss rss
    makeNewVars (Just vs : vss) (Just qs : qss) (Just rs : rss) =
      map (\(v, q, r) ->
              ((Just v, False),
                  Pattern { patternPre = patternPre q
                          , patternTy = patternTy r })
          ) (zip3 vs qs rs)
      ++ makeNewVars vss qss rss
    makeNewVars _ _ _ = error "unexpected arguments"

    pvarsImplicits :: Monad m =>
      [(VarName, Pattern)] -> TypeCheckT m [(VarName, Var)]
    pvarsImplicits qs =
      mapM (\(n, _) -> fmap (\i -> (n, mkVar i "_")) Env.freshVarId) qs

    pvarsArgs :: Monad m => [Maybe [Pattern]] -> TypeCheckT m [Maybe [Var]]
    pvarsArgs [] = return []
    pvarsArgs (Nothing : qss) =
      fmap (Nothing:) (pvarsArgs qss)
    pvarsArgs (Just qs : qss) = do
      qs' <- mapM (\_ -> fmap (flip mkVar "_") Env.freshVarId) qs
      qss' <- pvarsArgs qss
      return (Just qs' : qss')

    substTripleForce ::
      SubstMap -> SubstMap -> [CasePatternTriple] -> [CasePatternTriple]
    substTripleForce _ _ [] = []
    substTripleForce tsu bsu (((v, b), q) : qs) = do
      let qs' = substTripleForce tsu bsu qs
      let qty = substPreTerm bsu (substPreTerm tsu (patternTy q))
      let q' = q {patternTy = qty}
      case v of
        Nothing -> ((Nothing, b), q') : qs'
        Just v' ->
          let b' = b || IntMap.member (varId v') bsu
          in ((Just v', b'), q') : qs'

    makeCatchAllCases :: Monad m =>
      Int -> [(Loc, [CasePatternTriple], Maybe Term, [Var])] ->
      TypeCheckT m (Maybe ([(Loc, [CasePatternTriple], Maybe Term)], [Var]))
    makeCatchAllCases _ [] = return Nothing
    makeCatchAllCases idx ((lo, qs, t, _) : rss) = do
      ca <- makeCatchAllCases idx rss
      let (q, qs') = removeIdx idx qs
      case patternPre (snd q) of
        PatternVar v ->
          case ca of
            Nothing -> return (Just ([(lo, qs', t)], [v]))
            Just (ca', is') -> return (Just ((lo, qs', t) : ca', v : is'))
        _ -> return ca

    removeIdx :: Int -> [a] -> (a, [a])
    removeIdx 0 (x : xs) = (x, xs)
    removeIdx i (x : xs) =
      let (x', xs') = removeIdx (i-1) xs in (x', x : xs')
    removeIdx _ _ = error "unexpected input removeIdx"

verifyExprNoImplicits :: Monad m =>
  SubstMap -> IntMap (Loc, String) -> TypeCheckT m ()
verifyExprNoImplicits isu imps = do
  let imps' = IntMap.difference imps $ IntMap.restrictKeys imps (IntMap.keysSet isu)
  when (not (null imps')) $
    let imps'' = sortOn fst (map snd (IntMap.toList imps'))
    in mapM_ (\(lo, msg) -> err lo (Recoverable msg)) imps''

evalTcExpr :: Monad m => SubstMap -> PreTerm -> Expr -> TypeCheckT m Term
evalTcExpr su ty e = do
  (e', (s1, imps)) <- runStateT (tcExpr su ty e)
                        (IntMap.empty, ImpMap IntMap.empty Nothing)
  case imps of
    ImpMap _ (Just _) -> error "unexpected parent ImpMap"
    ImpMap imps' Nothing ->
      verifyExprNoImplicits s1 imps' >> return e'

scope :: Monad m => ExprT m Term -> ExprT m Term
scope e = fmap fst (dataScope (fmap (\e' -> (e', ())) e))

dataScope :: Monad m => ExprT m (Term, a) -> ExprT m (Term, a)
dataScope e = do
  st <- get
  (x, st') <- lift (Env.scope (runStateT e st))
  put st'
  return x

tcExpr :: Monad m => SubstMap -> PreTerm -> Expr -> ExprT m Term
tcExpr subst ty e = do
  rm <- lift Env.getRefMap
  case preTermLazyCod rm ty of
    Nothing -> exprCheck ty
    Just (cod, io) ->
      exprCheckLazy io cod `catchRecoverable` (\_ -> exprCheck ty)
  where
    exprCheck :: Monad m => PreTerm -> ExprT m Term
    exprCheck expectedTy = do
      e' <- doTcExpr False subst Nothing (Just expectedTy) e
      tcExprSubstUnify (exprLoc e) expectedTy (termTy e')
      su <- getExprSubst
      let se = substPreTerm su (termPre e')
      let st = substPreTerm su expectedTy
      return (mkTerm se st (termIo e'))

    exprCheckLazy :: Monad m => Bool -> PreTerm -> ExprT m Term
    exprCheckLazy io expectedTy = do
      e' <- doTcExpr False subst Nothing (Just expectedTy) e
      rm <- lift Env.getRefMap
      when (isJust (preTermLazyCod rm (termTy e'))) (throwError (Recoverable ""))
      tcExprSubstUnify (exprLoc e) expectedTy (termTy e')
      su <- getExprSubst
      let se = TermLazyFun io (substPreTerm su (termPre e'))
      let st = TermLazyArrow io (substPreTerm su expectedTy)
      return (mkTerm se st False)

doTcExprSubst :: Monad m =>
  Bool -> SubstMap -> Maybe (Either Expr (Expr, Expr)) -> Expr -> ExprT m Term
doTcExprSubst isTrial subst operandArg e = do
  e' <- doTcExpr isTrial subst operandArg Nothing e
  isu <- getExprSubst
  return (mkTerm (substPreTerm isu (termPre e'))
                 (substPreTerm isu (termTy e'))
                 (termIo e'))

-- The first Bool is a simple heuristic. It is indicating whether
-- only the root type of the expression is needed. It is certainly
-- possible to do better, fx by adding this to tcExpr as well.
doTcExpr :: Monad m =>
  Bool -> SubstMap -> Maybe (Either Expr (Expr, Expr)) ->
  Maybe PreTerm -> Expr -> ExprT m Term
doTcExpr _ _ _ _ (ExprUnitElem _) = return mkTermUnitElem
doTcExpr _ _ _ _ (ExprUnitTy _) = return mkTermUnitTy
doTcExpr _ _ _ _ (ExprTy _) = return mkTermTy
doTcExpr _ subst operandArg ty (ExprVar (lo, na0)) = do
  lift (checkRefNameValid (lo, na0))
  na <- operandVarName
  x <- lift (Env.lookup na)
  case x of
    Nothing -> lift $ err lo (Fatal $ "identifier not in scope " ++ quote na)
    Just x' -> do
      vt <- lift (varStatusTerm x')
      case termPre vt of
        (TermRef v _) -> makeImplicitApp v vt
        (TermData v) -> makeImplicitApp v vt
        (TermCtor v _) -> makeImplicitApp v vt
        _ -> do
          isu <- getExprSubst
          let e = substPreTerm subst (substPreTerm isu (termPre vt))
          let ety = substPreTerm subst (substPreTerm isu (termTy vt))
          return (mkTerm e ety False)
  where
    operandVarName :: Monad m => ExprT m VarName
    operandVarName =
      if not (isOperator na0)
      then return na0
      else
        if '\\' `elem` na0
        then return na0
        else
          if isRightAssocInfixOp na0
          then do
            rm <- lift Env.getRefMap
            case ty of
              Just ty' ->
                case preTermDomCod rm ty' of
                  Nothing -> unableToInferOperand
                  Just ([], _, _) -> unableToInferOperand
                  Just ([_], _, _) -> unableToInferOperand
                  Just (_ : (_, t) : _, _, _) -> operandVarNameFromType t
              Nothing -> operandVarNameFromArg
          else do
            rm <- lift Env.getRefMap
            case ty of
              Just ty' ->
                case preTermDomCod rm ty' of
                  Nothing -> unableToInferOperand
                  Just ([], _, _) -> unableToInferOperand
                  Just ((_, t) : _, _, _) -> operandVarNameFromType t
              Nothing -> operandVarNameFromArg

    checkOperand :: Monad m => Expr -> ExprT m Term
    checkOperand a = do
      s <- get
      r <- lift $ evalStateT (doTcExprSubst True subst Nothing a) s
      return r

    operandVarNameFromArg :: Monad m => ExprT m VarName
    operandVarNameFromArg = do
      if isRightAssocInfixOp na0
      then
        case operandArg of
          Nothing -> unableToInferOperand
          Just (Left _) -> unableToInferOperand
          Just (Right (_, a)) -> do
            a' <- checkOperand a
            operandVarNameFromType (termTy a')
      else
        case operandArg of
          Nothing -> unableToInferOperand
          Just (Left a) -> do
            a' <- checkOperand a
            operandVarNameFromType (termTy a')
          Just (Right (a, _)) -> do
            a' <- checkOperand a
            operandVarNameFromType (termTy a')

    operandVarNameFromType :: Monad m => PreTerm -> ExprT m VarName
    operandVarNameFromType aty = do
      --im <- lift Env.getImplicitMap
      --rm <- lift Env.getRefMap
      --let !() = trace (preTermToString im rm 0 aty) ()
      n <- lift (typeOperandName aty)
      case n of
        Nothing -> unableToInferOperand
        Just n' -> do
          let na0' = na0 ++ "\\"
          let na' = operandConcat na0 n'
          x1 <- lift $ Env.lookup na0'
          x2 <- lift $ Env.lookup na'
          when (isJust x1 && isJust x2)
            (lift $ err lo (Fatal $
                             "multiple candidates for operator "
                             ++ quote na0
                             ++ ", use operand type argument "
                             ++ quote (na0 ++ "\\...")
                             ++ " to disambiguate"))
          if isJust x1
          then return na0'
          else return na'

    unableToInferOperand :: Monad m => ExprT m VarName
    unableToInferOperand = do
      let na0' = na0 ++ "\\"
      x <- lift $ Env.lookup na0'
      when (isNothing x)
        (lift $ err lo (Recoverable $ "unable to infer operand type of " ++ quote na0))
      return na0'

    makeImplicitApp :: Monad m => Var -> Term -> ExprT m Term
    makeImplicitApp v t = do
      imps0 <- lift (Env.forceLookupImplicit (varId v))
      if null imps0
        then return t
        else do
          imps1 <- mapM getImplicit imps0
          let su = IntMap.fromList (map (\(v0, x) -> (varId v0, x)) imps1)
          let ias = map (\(v0, x) -> (varName v0, x)) imps1
          let a = TermImplicitApp False (termPre t) ias
          let aty = substPreTerm su (termTy t)
          return (mkTerm a aty False)

    getImplicit :: Monad m => (Var, PreTerm) -> ExprT m (Var, PreTerm)
    getImplicit (v, _) = do
      i <- lift Env.freshVarId
      let v' = mkVar i ('.' : varName v)
      impMapInsert i lo $
        "unable to infer implicit argument " ++ varName v
      return (v, TermVar True v')
doTcExpr _ subst _ ty (ExprFun lo as body) = do
  scope $ do
    case ty of
      Nothing -> do
        implicits <- mapM insertImplicit as
        ats <- mapM insertUnknownUntypedParam (zip as implicits)
        dom' <- mapM checkParam (zip as ats)
        body' <- doTcExpr False subst Nothing Nothing body
        let io = termIo body'
        let fte = TermFun [] io (Just (length dom'))
                    (CaseLeaf (map fst dom') io (termPre body') [])
        let mdom = map (\(d1, d2) -> (Just d1, d2)) dom'
        let fty = TermArrow io mdom (termTy body')
        return (mkTerm fte fty False)
      Just ty' -> do
        rm <- lift Env.getRefMap
        case preTermDomCod rm ty' of
          Nothing -> do
            implicits <- mapM insertImplicit as
            ats <- mapM insertUnknownUntypedParam (zip as implicits)
            dom' <- mapM checkParam (zip as ats)
            body' <- doTcExpr False subst Nothing Nothing body
            let io = termIo body'
            let fte = TermFun [] io (Just (length dom'))
                        (CaseLeaf (map fst dom') io (termPre body') [])
            let mdom = map (\(d1, d2) -> (Just d1, d2)) dom'
            let fty = TermArrow io mdom (termTy body')
            tcExprSubstUnify lo ty' fty
            return (mkTerm fte fty False)
          Just (dom, cod, io) -> do
            when (length dom /= length as)
              (lift $ err lo (Fatal $
                               "expected " ++ show (length dom)
                               ++ " argument(s), but function has "
                               ++ show (length as)))
            mapM_ insertUnknownTypedParam as
            rm0 <- lift Env.getRefMap
            let as' = dependencyOrderArgs (preTermVars rm0) dom as
            (su0, dom0) <- insertTypedParams IntMap.empty as'
            let dom' = map snd (sortOn fst dom0)
            let cod' = substPreTerm su0 cod
            body' <- tcExpr subst cod' body
            let bodyIo = termIo body'
            when (bodyIo && not io)
              (lift $ err lo (Recoverable $
                  "type of function is effectful, but expected "
                  ++ "pure function type"))
            let fte = TermFun [] bodyIo (Just (length dom'))
                        (CaseLeaf (map fst dom') bodyIo (termPre body') [])
            let mdom = map (\(d1, d2) -> (Just d1, d2)) dom'
            let fty = TermArrow io mdom cod'
            return (mkTerm fte fty False)
  where
    insertImplicit :: Monad m =>
      VarListElem -> ExprT m (Maybe PreTerm)
    insertImplicit ((vlo, vna), Nothing) = do
      i <- lift Env.freshVarId
      let e = TermVar True (mkVar i ('.' : vna ++ ".Ty"))
      impMapInsert i vlo $ "unable to infer type of " ++ quote vna
      return (Just e)
    insertImplicit (_, Just _) = return Nothing

    insertUnknownUntypedParam :: Monad m =>
      (VarListElem, Maybe PreTerm) -> ExprT m (Maybe (Var, PreTerm))
    insertUnknownUntypedParam (((vlo, vna), Nothing), Just e) = do
      v <- lift (insertNonblankFreshVariable (vlo, vna) e)
      return (Just (v, e))
    insertUnknownUntypedParam (((vlo, vna), Just e), _) = do
      lift (insertUnknownVar subst (vlo, vna) (Just e))
      return Nothing
    insertUnknownUntypedParam _ = error "unexpected case"

    checkParam :: Monad m =>
      (VarListElem, Maybe (Var, PreTerm)) -> ExprT m (Var, PreTerm)
    checkParam ((_, Nothing), Nothing) =
      error "expected function explicit type or implicit type"
    checkParam ((_, Nothing), Just x) = return x
    checkParam (((vlo, vna), Just e), _)
      | vna == "_" = do
          e' <- tcExpr subst TermTy e
          lift (verifyNoIoEscapeFromType vlo vna e')
          i <- lift Env.freshVarId
          let v = mkVar i "_"
          return (v, termPre e')
      | True = do
          pa <- lift (markInProgress False (vlo, vna))
          case pa of
            Nothing -> do
              e' <- tcExpr subst TermTy e
              lift (verifyNoIoEscapeFromType vlo vna e')
              i <- lift Env.freshVarId
              let v = mkVar i vna
              lift $ updateToStatusTerm vna
                        (mkTerm (TermVar False v) (termPre e') False)
              return (v, termPre e')
            Just (Term {termPre = TermVar _ v, termTy = tt}) ->
              return (v, tt)
            _ ->
              error $ "not a variable term " ++ quote vna

    insertUnknownTypedParam :: Monad m => VarListElem -> ExprT m ()
    insertUnknownTypedParam ((vlo, vna), Nothing) =
      lift (insertUnknownVar subst (vlo, vna) Nothing)
    insertUnknownTypedParam ((vlo, vna), Just e) =
      lift (insertUnknownVar subst (vlo, vna) (Just e))

    insertTypedParams :: Monad m =>
      SubstMap -> [(Int, (Maybe Var, PreTerm), VarListElem)] ->
      ExprT m (SubstMap, [(Int, (Var, PreTerm))])
    insertTypedParams su [] = return (su, [])
    insertTypedParams su ((idx, (v, t), ((vlo, vna), e)) : xs) = do
      pa <- if vna == "_"
              then return Nothing
              else lift (markInProgress False (vlo, vna))
      let t' = substPreTerm su t
      case e of
        Nothing -> return ()
        Just e' -> do
          t0 <- case pa of
                  Nothing -> do
                    e'' <- tcExpr (IntMap.union subst su) TermTy e'
                    lift (verifyNoIoEscapeFromType vlo vna e'')
                    return (termPre e'')
                  Just pa' ->
                    return (termTy pa')
          -- Should not implicit coerce to lazy here, since the
          -- thing of type t' is a function parameter (variable).
          tcExprSubstUnify (exprLoc e') t' t0
      i <- lift Env.freshVarId
      let var = mkVar i vna
      let tvar = TermVar False var
      when (vna /= "_")
        (lift $ updateToStatusTerm vna (mkTerm tvar t' False))
      let su' = case v of
                  Nothing -> su
                  Just v' -> IntMap.insert (varId v') tvar su 
      (su'', ts) <- insertTypedParams su' xs
      return (su'', (idx, (var, t')) : ts)
doTcExpr _ subst _ Nothing (ExprLazyFun _ e) = do
  e' <- doTcExpr False subst Nothing Nothing e
  let io = termIo e'
  let lf = TermLazyFun io (termPre e')
  let la = TermLazyArrow io (termTy e')
  return (mkTerm lf la False)
doTcExpr isTrial subst _ (Just ty) (ExprLazyFun lo e) = do
  rm <- lift Env.getRefMap
  case preTermLazyCod rm ty of
    Nothing -> doTcExpr isTrial subst Nothing Nothing (ExprLazyFun lo e)
    Just (cod, io) -> do
      e' <- tcExpr subst cod e
      let bodyIo = termIo e'
      when (bodyIo && not io)
        (lift $ err lo (Fatal $
          "type of lazy is effectful, but expected pure lazy"))
      let lf = TermLazyFun bodyIo (termPre e')
      return (mkTerm lf ty False)
doTcExpr _ subst _ _ (ExprArrow _ io dom cod) = do
  scope $ do
    mapM_ insertUnknownParam dom
    dom0 <- mapM checkParam dom
    let (dom', iod) = foldr (\(v,p,b) (xs,c) ->
                                ((v,p) : xs, b || c)) ([], False) dom0
    cod' <- tcExpr subst TermTy cod
    let ar = TermArrow io dom' (termPre cod')
    return (mkTerm ar TermTy (iod || termIo cod'))
  where
    insertUnknownParam :: Monad m => ExprListTypedElem -> ExprT m ()
    insertUnknownParam (Left _) = return ()
    insertUnknownParam (Right ((vlo, vna), e)) =
      lift (insertUnknownVar subst (vlo, vna) (Just e))

    checkParam :: Monad m =>
      ExprListTypedElem -> ExprT m (Maybe Var, PreTerm, Bool)
    checkParam (Left e) = do
      e' <- tcExpr subst TermTy e
      return (Nothing, termPre e', termIo e')
    checkParam (Right ((vlo, vna), e))
      | vna == "_" = checkParam (Left e)
      | True = do
          pa <- lift (markInProgress False (vlo, vna))
          case pa of
            Nothing -> do
              e' <- tcExpr subst TermTy e
              i <- lift Env.freshVarId
              let v = mkVar i vna
              lift $ updateToStatusTerm vna
                        (mkTerm (TermVar False v) (termPre e') False)
              return (Just v, termPre e', termIo e')
            Just (Term {termPre = TermVar _ v, termTy = tt, termIo = ti}) ->
              return (Just v, tt, ti)
            _ ->
              error $ "not a variable term " ++ quote vna
doTcExpr _ subst _ _ (ExprLazyArrow _ io e) = do
  e' <- tcExpr subst TermTy e
  return (mkTerm (TermLazyArrow io (termPre e')) TermTy (termIo e'))
doTcExpr isTrial subst _ ty (ExprApp f args0) = do
  let (partials, args) = updateBlanks args0 0
  if null partials
  then
    case args of
      [] -> doTcExprApp isTrial subst Nothing ty f args
      [a] -> doTcExprApp isTrial subst (Just (Left a)) ty f args
      a1 : a2 : _ -> doTcExprApp isTrial subst (Just (Right (a1, a2))) ty f args
  else doTcExpr isTrial subst Nothing ty (ExprFun (exprLoc f) partials (ExprApp f args))
  where
    updateBlanks :: [Expr] -> Int -> (VarList, [Expr])
    updateBlanks [] _ = ([], [])
    updateBlanks (ExprVar (vlo, "_") : es) i =
      let (bs, es') = updateBlanks es (i + 1)
          vna = '#' : show i
      in (((vlo, vna), Nothing) : bs, ExprVar (vlo, vna) : es')
    updateBlanks (e : es) i =
      let (bs, es') = updateBlanks es i
      in (bs, e : es')
doTcExpr isTrial subst operandArg _ (ExprImplicitApp f args) = do
  f' <- doTcExprSubst isTrial subst operandArg f
  opn <- if isTrial then return Nothing else lift (typeOperandName (termTy f'))
  if isTrial && isJust opn
  then return (mkTerm TermEmpty (termTy f') False)
  else do
    im <- lift Env.getImplicitMap
    rm <- lift Env.getRefMap
    (r, v, ias) <- case termPre f' of
                    TermImplicitApp False r@(TermRef v _) a -> return (r, v, a)
                    TermImplicitApp False r@(TermData v) a -> return (r, v, a)
                    TermImplicitApp False r@(TermCtor v _) a -> return (r, v, a)
                    _ -> lift (err (exprLoc f) (
                                Recoverable $
                                "cannot apply term\n"
                                ++ preTermToString im rm defaultExprIndent (termPre f')
                                ++ "\nto implicit argument(s)"))
    tys <- lift (Env.forceLookupImplicit (varId v))
    let ias' = Map.fromList ias
    let su = foldl (\m (i, _) ->
                      IntMap.insert (varId i) (fromJust (Map.lookup (varName i) ias')) m
                   ) IntMap.empty tys
    let z = zipWith (\x y ->
                      (fst y, (snd y, substPreTerm su (snd x)))
                    ) tys ias
    let m = Map.fromList z
    io <- unifyImplicits (Map.keysSet m) m args
    return (f' {termPre = TermImplicitApp True r ias,
                termIo = termIo f' || io})
  where
    unifyImplicits :: Monad m =>
      Set VarName -> Map VarName (PreTerm, PreTerm) ->
      [((Loc, String), Expr)] -> ExprT m Bool
    unifyImplicits _ _ [] = return False
    unifyImplicits remain ias (((lo, na), e) : es) = do
      case Map.lookup na ias of
        Nothing -> lift (err lo (Fatal $ "unexpected implicit argument name "
                                         ++ quote na))
        Just (a, ty) ->
          if Set.member na remain
            then do
              e0 <- tcExpr subst ty e
              e' <- runUnify a e0
              io <- unifyImplicits (Set.delete na remain) ias es
              return (io || termIo e')
            else
              lift (err lo (Fatal $ "implicit argument " ++ quote na
                                    ++ " is given multiple times"))
      where
        msgPrefix :: Monad m => ExprT m String
        msgPrefix = do
          return $
            "failed unifying values for implicit argument " ++ quote na
        runUnify :: Monad m => PreTerm -> Term -> ExprT m Term
        runUnify a e0 = do
          (runExprUnifResult lo False msgPrefix a (termPre e0) >> return e0)
            `catchRecoverable` runUnifyLazy a e0
        runUnifyLazy :: Monad m => PreTerm -> Term -> String -> ExprT m Term
        runUnifyLazy a e0 msg = do
          let e1 = Term { termPre = TermLazyFun (termIo e0) (termPre e0)
                        , termTy = TermLazyArrow (termIo e0) (termTy e0)
                        , termNestedDefs = []
                        , termIo = False }
          runExprUnifResult lo False msgPrefix a (termPre e1)
            `catchRecoverable` (\_ -> throwError (Recoverable msg))
          return e1
doTcExpr isTrial subst _ _ (ExprLazyApp e) = do
  e' <- doTcExprSubst isTrial subst Nothing e
  if not isTrial
  then doMakeLazyApp e'
  else do
    rm <- lift Env.getRefMap
    case preTermLazyCod rm (termTy e') of
      Nothing -> doMakeLazyApp e'
      Just (cod, _) -> do
        opn <- lift (typeOperandName cod)
        if isNothing opn
        then doMakeLazyApp e'
        else return (mkTerm TermEmpty cod False)
  where
    doMakeLazyApp :: Monad m => Term -> ExprT m Term
    doMakeLazyApp e' = do
      rm <- lift Env.getRefMap
      im <- lift Env.getImplicitMap
      case preTermLazyCod rm (termTy e') of
        Nothing ->
          lift $ err (exprLoc e) (Recoverable $
            "expected expression to have lazy function type, "
            ++ "but type is\n"
            ++ preTermToString im rm defaultExprIndent (termTy e'))
        Just (ty', io) -> do
          return (mkTerm (TermLazyApp io (termPre e')) ty' (termIo e' || io))
doTcExpr _ subst _ ty (ExprSeq (Left e1) e2) =
  let p1 = ParsePatternUnit (exprLoc e1)
  in doTcExpr False subst Nothing ty (ExprSeq (Right (p1, e1)) e2)
doTcExpr _ subst _ ty (ExprSeq (Right (p1, e1)) e2) =
  doTcExpr False subst Nothing ty (ExprCase (patternLoc p1) e1 [Right (p1, e2)])
doTcExpr _ subst _ ty (ExprCase lo expr cases) = do
  expr' <- doTcExprSubst False subst Nothing expr
  newpids <- lift Env.getNextVarId
  (cases', ty0) <- checkCases newpids (termTy expr') cases ty
  case ty0 of
    Nothing -> lift (err lo (Recoverable "unable to infer type of expression"))
    Just ty' -> do
      (ct, io) <- lift (casesToCaseTree cases')
      let c = TermCase (termPre expr') ct
      return (mkTerm c ty' (termIo expr' || io))
  where
    checkCases :: Monad m =>
      VarId -> PreTerm -> [OfCase] -> Maybe PreTerm ->
      ExprT m ([(Loc, [CasePatternTriple], Maybe Term)], Maybe PreTerm)
    checkCases _ _ [] expectedTy = return ([], expectedTy)
    checkCases newpids t ((Left p) : ps) expectedTy = do
      c <- doCheckCase newpids t p Nothing expectedTy
      (cs, ty') <- checkCases newpids t ps expectedTy
      return (c : cs, ty')
    checkCases newpids t ((Right (p, e)) : ps) Nothing = do
      c@(_, _, d) <- doCheckCase newpids t p (Just e) Nothing
      (cs, ty') <- case d of
                    Just d' ->
                      checkCases newpids t ps (Just (termTy d'))
                    Nothing ->
                      error "unexpected missing case in case-expression"
      return (c : cs, ty')
    checkCases newpids t ((Right (p, e)) : ps) expectedTy = do
      c <- doCheckCase newpids t p (Just e) expectedTy
      (cs, ty') <- checkCases newpids t ps expectedTy
      return (c : cs, ty')

    doCheckCase :: Monad m =>
      VarId -> PreTerm -> ParsePattern -> Maybe Expr -> Maybe PreTerm ->
      ExprT m (Loc, [CasePatternTriple], Maybe Term)
    doCheckCase newpids t p (Just e) expectedTy = do
      (e', tr) <- dataScope $ do
        (su, p') <- lift (tcPattern newpids t p)
        isu0 <- getExprSubst
        let isu1 = IntMap.restrictKeys su (IntMap.keysSet isu0)
        boundids <- lift Env.getNextVarId
        isu <- lift (runExceptT (mergeExprUnifMaps boundids isu0 isu1))
        case isu of
          Right x -> putExprSubst x
          Left msg -> lift (err (exprLoc e)
                           (Recoverable $ "unification error, " ++ msg))
        b <- case expectedTy of
              Nothing ->
                doTcExprSubst False (IntMap.union su subst) Nothing e
              Just ty' ->
                tcExpr (IntMap.union su subst) (substPreTerm su ty') e
        checkNoVariableEscape (patternLoc p) (patternPre p') (termTy b)
        return (b, patternToTriple p')
      return (patternLoc p, tr, Just e')
    doCheckCase newpids t p Nothing _ =
      lift . Env.scope $ do
        (_, p') <- tcPattern newpids t p
        return (patternLoc p, patternToTriple p', Nothing)

    patternToTriple :: Pattern -> [CasePatternTriple]
    patternToTriple p = [((Nothing, False), p)]

    checkNoVariableEscape ::
      Monad m => Loc -> PrePattern -> PreTerm -> ExprT m ()
    checkNoVariableEscape plo p t = do
      let vp = prePatternVars p
      rm <- lift Env.getRefMap
      let vt = preTermVars rm t
      let s = IntSet.intersection vp vt
      when (not (IntSet.null s))
        (lift $ err plo (Fatal "pattern variable(s) are escaping scope"))

doTcExprApp :: Monad m =>
  Bool -> SubstMap -> Maybe (Either Expr (Expr, Expr)) ->
  Maybe PreTerm -> Expr -> [Expr] -> ExprT m Term
doTcExprApp isTrial subst operandArg ty f args = do
  f' <- doTcExprSubst isTrial subst operandArg f
  if not isTrial
  then doMakeTermApp f'
  else do
    rm <- lift Env.getRefMap
    case preTermDomCod rm (termTy f') of
      Nothing -> doMakeTermApp f'
      Just (_, cod, _) -> do
        asp <- getArgSplit f'
        if isNothing asp
        then makeTrivialApp f' cod
        else
          case preTermDomCod rm cod of
            Nothing -> doMakeTermApp f'
            Just (_, cod', _) -> makeTrivialApp f' cod'
  where
    makeTrivialApp :: Monad m => Term -> PreTerm -> ExprT m Term
    makeTrivialApp f' cod = do
      opn <- lift (typeOperandName cod)
      if isNothing opn
      then doMakeTermApp f'
      else return (mkTerm TermEmpty cod False)

    getArgSplit :: Monad m => Term -> ExprT m (Maybe (Expr, [Expr]))
    getArgSplit f' =
      case args of
        a1 : a2 : args' ->
          if isPostfixExpr f
          then do
            r <- lift Env.getRefMap
            case preTermDomCod r (termTy f') of
              Just ([_], _, _) -> do
                return (Just (a1, a2 : args'))
              _ -> return Nothing
          else return Nothing
        _ -> return Nothing

    doMakeTermApp :: Monad m => Term -> ExprT m Term
    doMakeTermApp f' = do
      asp <- getArgSplit f'
      case asp of
        Nothing -> makeTermApp subst (exprLoc f) ty f' args
        Just (a1, args') -> do
          e1 <- makeTermApp subst (exprLoc f) Nothing f' [a1]
          isu <- getExprSubst
          let e1' = mkTerm (substPreTerm isu (termPre e1))
                      (substPreTerm isu (termTy e1)) (termIo e1)
          makeTermApp subst (exprLoc f) ty e1' args'

    isPostfixExpr :: Expr -> Bool
    isPostfixExpr (ExprVar (_, vna)) = isPostfixOp vna && not ('\\' `elem` vna)
    isPostfixExpr (ExprImplicitApp v@(ExprVar _) _) = isPostfixExpr v
    isPostfixExpr _ = False

makeTermApp :: Monad m =>
  SubstMap -> Loc ->
  Maybe PreTerm -> Term -> [Expr] -> ExprT m Term
makeTermApp subst flo ty f' args = do
  r <- lift Env.getRefMap
  im <- lift Env.getImplicitMap
  let dc = preTermDomCod r (termTy f')
  case dc of
    Nothing ->
      lift $ err flo
        (Recoverable $
          "expected expression to have function type, "
          ++ "but type is\n"
          ++ preTermToString im r defaultExprIndent (termTy f'))
    Just (d', c', io) -> do
      when (length d' /= length args)
        (lift $ err flo
                  (Fatal $
                    "expected " ++ show (length d') ++ " argument(s), "
                    ++ "but given " ++ show (length args)))
      let args' = dependencyOrderArgs (preTermVars r) d' args
      let args'' = map (\(i, (v, p), e) -> (i, Left (v, p, e))) args'
      let domVars = getDomVars d'
      (su, ps, io') <- applyArgsLoop domVars c' False IntMap.empty False args''
                        `catchRecoverable`
                            (\_ -> do
                                    _ <- applyArgs False args''
                                    _ <- unifyType domVars c' IntMap.empty
                                    error "expected applyArgs or unifyType to throw error")
      let ps' = map snd (sortOn fst ps)
      let c = substPreTerm su c'
      let e = TermApp io (termPre f') ps'
      return (mkTerm e c (termIo f' || io || io'))
  where
    unifyType :: Monad m => IntSet -> PreTerm -> SubstMap -> ExprT m ()
    unifyType domVars cod su = do
      case ty of
        Just ty' -> do
          let cod' = substPreTerm su cod
          rm <- lift Env.getRefMap
          let fs = preTermVars rm cod'
          if IntSet.null (IntSet.intersection fs domVars)
          then tcExprSubstUnify flo ty' cod'
          else return ()
        Nothing -> return ()

    applyArgsLoop :: Monad m =>
      IntSet -> PreTerm -> Bool -> SubstMap -> Bool ->
      [(Int, Either (Maybe Var, PreTerm, Expr) PreTerm)] ->
      ExprT m (SubstMap, [(Int, PreTerm)], Bool)
    applyArgsLoop domVars cod unified su io as = do
      unifyType domVars cod su `catchRecoverable` (\_ -> return ())
      case projArgs as of
        Just xs -> return (su, xs, io)
        Nothing -> do
          (progress, su', as', io') <- applyArgs True as
          if progress
          then
            applyArgsLoop domVars cod unified (IntMap.union su su') (io || io') as'
          else
            if unified
            then throwError (Recoverable "")
            else applyArgsLoop domVars cod True su io as
      where
        projArgs :: 
          [(Int, Either (Maybe Var, PreTerm, Expr) PreTerm)] ->
          Maybe [(Int, PreTerm)]
        projArgs [] = Just []
        projArgs ((idx, Right t) : xs) = fmap ((idx, t) :) (projArgs xs)
        projArgs ((_, Left _) : _) = Nothing

    getDomVars :: [(Maybe Var, PreTerm)] -> IntSet
    getDomVars [] = IntSet.empty
    getDomVars ((Nothing, _) : vs) = getDomVars vs
    getDomVars ((Just v, _) : vs) = IntSet.insert (varId v) (getDomVars vs)

    applyArgs :: Monad m =>
      Bool -> [(Int, Either (Maybe Var, PreTerm, Expr) PreTerm)] ->
      ExprT m
        (Bool, SubstMap, [(Int, Either (Maybe Var, PreTerm, Expr) PreTerm)], Bool)
    applyArgs _ [] = return (False, IntMap.empty, [], False)
    applyArgs cat ((idx, Right t) : xs) = do
      (progress, m, xs', io) <- applyArgs cat xs
      return (progress, m, (idx, Right t) : xs', io)
    applyArgs cat ((idx, Left (Nothing, p, e)) : xs) = do
      (e', eio) <- applyTc cat p e
      (progress, su, es', io) <- applyArgs cat xs
      return (progress || isRight e', su, (idx, e') : es', io || eio)
    applyArgs cat ((idx, Left (Just v, p, e)) : xs) = do
      (e', eio) <- applyTc cat p e
      r <- lift Env.getRefMap
      let xs' = case e' of
                  Right e'' -> substArgs r (IntMap.singleton (varId v) e'') xs
                  Left _ -> xs
      (progress, su, es', io) <- applyArgs cat xs'
      let su' = case e' of
                  Right e'' -> IntMap.insert (varId v) e'' su
                  Left _ -> su
      return (progress || isRight e', su', (idx, e') : es', io || eio)

    applyTc True p e = do
      isu <- getExprSubst
      let p' = substPreTerm isu p
      fmap (\d -> (Right (termPre d), termIo d)) (tcExpr subst p' e)
        `catchRecoverable` (\_ -> return (Left (Nothing, p, e), False))
    applyTc False p e = do
      isu <- getExprSubst
      let p' = substPreTerm isu p
      fmap (\d -> (Right (termPre d), termIo d)) (tcExpr subst p' e)

    substArgs ::
      RefMap -> SubstMap ->
      [(Int, Either (Maybe Var, PreTerm, Expr) PreTerm)] ->
      [(Int, Either (Maybe Var, PreTerm, Expr) PreTerm)]
    substArgs _ _ [] = []
    substArgs r m ((idx, Right t) : xs) =
      (idx, Right t) : substArgs r m xs
    substArgs r m ((idx, Left (v, p, e)) : xs) =
      (idx, Left (v, substPreTerm m p, e)) : substArgs r m xs

tcPattern :: Monad m =>
  VarId -> PreTerm -> ParsePattern -> TypeCheckT m (SubstMap, Pattern)
tcPattern newpids ty p = do
  (su, p')  <- doTcPattern False False newpids ty p
  im <- Env.getImplicitMap
  rm <- Env.getRefMap
  --let !() = trace ("type: " ++ preTermToString im rm 0 (patternTy p')) ()
  when (patternPreCanApply (patternPre p')
        && isArrowType rm (patternTy p'))
    (err (patternLoc p) (Fatal $
      "pattern\n"
      ++ prePatternToString im rm defaultExprIndent (patternPre p')
      ++ "\nis missing argument(s)"))
  return (su, p')
  where
    isArrowType :: RefMap -> PreTerm -> Bool
    isArrowType rm t = isArrowType' (preTermNormalize rm t)
    isArrowType' :: PreTerm -> Bool
    isArrowType' (TermArrow _ _ _) = True
    isArrowType' (TermLazyArrow _ _) = True
    isArrowType' _ = False

makePatternApp :: Monad m =>
  Loc -> VarId -> PreTerm -> (SubstMap, Pattern) -> [ParsePattern] ->
  TypeCheckT m (SubstMap, Pattern)
makePatternApp flo newpids ty (fsu, f') pargs = do
  r <- Env.getRefMap
  im <- Env.getImplicitMap
  when (not $ patternPreCanApply (patternPre f'))
    (err flo (Recoverable $
      "unable to match pattern with inferred type\n"
      ++ preTermToString im r defaultExprIndent (patternTy f')))
  let dc = preTermDomCod r (patternTy f')
  case dc of
    Nothing ->
      err flo (Recoverable $
        "unable to match pattern with inferred type\n"
        ++ preTermToString im r defaultExprIndent (patternTy f'))
    Just (dom0, cod, io) -> do
      let !() = assert (not io) ()
      when (length dom0 /= length pargs)
        (err flo
          (Fatal $
            "expected " ++ show (length dom0) ++ " argument(s), "
            ++ "but given " ++ show (length pargs)))
      let (fcod, _) = preTermFinalCod r cod
      un <- runExceptT (patternUnify2 newpids fcod ty)
      let dom' = case un of
                  Left _ -> dom0
                  Right dsu -> map (\(d, t) -> (d, substPreTerm dsu t)) dom0
      let args = dependencyOrderArgs (preTermVars r) dom' pargs
      (tsu, bsu, ps) <- tcPatternArgs newpids IntMap.empty fsu args
      let c = substPreTerm tsu cod
      r0 <- Env.getRefMap
      psu <- if isNothing (preTermDomCod r0 cod)
                && isNothing (preTermLazyCod r0 cod)
              then do
                m <- tcPatternUnify newpids flo ty c
                tcMergePatUnifMaps newpids flo m bsu
              else
                return bsu
      let ps' = map (patternPre . snd . snd) (sortOn fst ps)
      let p = Pattern {patternPre = PatternApp (patternPre f') ps',
                       patternTy = c}
      return (psu, p)

doTcPattern :: Monad m =>
  Bool -> Bool -> VarId -> PreTerm -> ParsePattern -> TypeCheckT m (SubstMap, Pattern)
doTcPattern _ _ newpids ty (ParsePatternApp f pargs) = do
  f' <- doTcPattern False True newpids ty f
  case pargs of
    a1 : a2 : pargs' ->
      if isPostfixPattern f
      then do
        r <- Env.getRefMap
        case preTermDomCod r (patternTy (snd f')) of
          Just ([_], _, _) -> do
            f'' <- makePatternApp (patternLoc f) newpids ty f' [a1]
            makePatternApp (patternLoc f) newpids ty f'' (a2 : pargs')
          _ -> makePatternApp (patternLoc f) newpids ty f' pargs
      else makePatternApp (patternLoc f) newpids ty f' pargs
    _ -> makePatternApp (patternLoc f) newpids ty f' pargs
  where
    isPostfixPattern :: ParsePattern -> Bool
    isPostfixPattern (ParsePatternVar (_, vna)) =
      isPostfixOp vna && not ('\\' `elem` vna)
    isPostfixPattern (ParsePatternImplicitApp v@(ParsePatternVar _) _) =
      isPostfixPattern v
    isPostfixPattern _ = False

doTcPattern _ hasApp newpids ty (ParsePatternImplicitApp f pargs) = do
  (fsu, f') <- doTcPattern True hasApp newpids ty f
  im <- Env.getImplicitMap
  rm <- Env.getRefMap
  (r, v, ias) <- case patternPre f' of
                  PatternImplicitApp False r@(PatternCtor v _) a -> return (r, v, a)
                  _ -> err (patternLoc f) (Fatal $
                              "cannot apply pattern\n"
                              ++ prePatternToString im rm
                                  defaultExprIndent (patternPre f')
                              ++ "\nto implicit argument(s)")
  tys0 <- Env.forceLookupImplicit (varId v)
  let (fcod, _) = preTermFinalCod rm (patternTy f')
  un <- runExceptT (patternUnify2 newpids fcod ty)
  let tys = case un of
              Left _ -> tys0
              Right dsu -> map (\(d, t) -> (d, substPreTerm dsu t)) tys0
  pnames <- getImplicitNames (Set.fromList (map (varName . fst) tys)) Set.empty pargs
  let ias' = Map.fromList ias
  let su = foldl (\m (i, _) ->
                    IntMap.insert (varId i)
                      (prePatternToPreTerm
                        (fromJust (Map.lookup (varName i) ias'))) m
                 ) IntMap.empty tys
  let z = foldr (\(i, x) a ->
                  if Set.member (varName (fst x)) pnames
                    then (Just (patternVarVar (snd i)),
                          substPreTerm su (snd x)) : a
                    else a
                ) [] (zip ias tys)
  let pmap = Map.fromList (map (\(x,y) -> (snd x, y)) pargs)
  let (pns, pargs') = foldr (\(x, _) (a1, a2) ->
                          if Set.member x pnames
                            then (x : a1,
                                  fromJust (Map.lookup x pmap) : a2)
                            else (a1, a2)
                        ) ([], []) ias
  let args = dependencyOrderArgs (preTermVars rm) z pargs'
  (tsu, bsu, ps) <- tcPatternArgs newpids IntMap.empty fsu args
  let pt = patternTy f'
  let c = substPreTerm tsu pt
  r0 <- Env.getRefMap
  psu <- if isNothing (preTermDomCod r0 pt)
            && isNothing (preTermLazyCod r0 pt)
          then do
            m <- tcPatternUnify newpids (patternLoc f) ty c
            tcMergePatUnifMaps newpids (patternLoc f) m bsu
          else
            return bsu
  let ps' = map (patternPre . snd . snd) (sortOn fst ps)
  let a = PatternImplicitApp True r (remakeArgs (zip pns ps') ias)
  let p = Pattern {patternPre = a, patternTy = c}
  return (psu, p)
  where
    getImplicitNames :: Monad m =>
      Set VarName -> Set VarName ->
      [((Loc, String), ParsePattern)] -> TypeCheckT m (Set VarName)
    getImplicitNames _ acc [] = return acc
    getImplicitNames allnames acc (((lo, na), _) : ps) = do
      when (Set.member na acc)
        (err lo (Fatal $ "implicit argument " ++ quote na
                  ++ " given multiple times"))
      when (not (Set.member na allnames))
        (err lo (Fatal $ "unexpected implicit argument name " ++ quote na))
      getImplicitNames allnames (Set.insert na acc) ps

    patternVarVar :: PrePattern -> Var
    patternVarVar (PatternVar v) = v
    patternVarVar _ = error "expected pattern to be variable pattern"

    remakeArgs ::
      [(VarName, PrePattern)] ->
      [(VarName, PrePattern)] -> [(VarName, PrePattern)]
    remakeArgs [] vs = vs
    remakeArgs (p:ps) (v:vs) =
      if fst p == fst v
        then p : remakeArgs ps vs
        else v : remakeArgs (p:ps) vs
    remakeArgs _ _ = error "unexpected arguments to remakeArgs"
doTcPattern _ _ newpids ty (ParsePatternLazyApp f) = do
  (fsu, f') <- doTcPattern False True newpids ty f
  r <- Env.getRefMap
  im <- Env.getImplicitMap
  when (not $ patternPreCanApply (patternPre f'))
    (err (patternLoc f) (Recoverable $
      "unable to match pattern with inferred type\n"
      ++ preTermToString im r defaultExprIndent (patternTy f')))
  let c = preTermLazyCod r (patternTy f')
  case c of
    Nothing ->
      err (patternLoc f) (Recoverable $
        "unable to match pattern with inferred type\n"
        ++ preTermToString im r defaultExprIndent (patternTy f'))
    Just (cod, io) -> do
      let !() = assert (not io) ()
      let p = Pattern {patternPre = PatternLazyApp (patternPre f'),
                       patternTy = cod}
      return (fsu, p)
doTcPattern hasImplicitApp hasApp newpids ty (ParsePatternVar (lo, na0)) = do
  na <- if not (isOperator na0) || '\\' `elem` na0
          then return na0
          else getDataCtorMatching
  x <- Env.lookup na
  case x of
    Just (StatusTerm (Term {termPre = TermCtor v did, termTy = ty'})) -> do
      checkCtorType v did ty'
      --return (su, Pattern {patternPre = PatternCtor v did, patternTy = ty'})
    Just x'@(StatusUnknownCtor _ _ _ _) -> do
      (v, did, ty') <- ctorFromVarStatus x'
      checkCtorType v did ty'
      --return (su, Pattern {patternPre = PatternCtor v did, patternTy = ty'})
    Just x'@(StatusInProgress True _) -> do
      _ <- ctorFromVarStatus x'
      error "this should be unreachable"
    _ -> do
      im <- Env.getImplicitMap
      rm <- Env.getRefMap
      if hasImplicitApp || hasApp
      then err lo (Recoverable $ "expected " ++ quote na
                               ++ " to be constructor of\n"
                               ++ preTermToString im rm defaultExprIndent ty)
      else do
        v <- insertNonblankFreshVariable (lo, na) ty
        return (IntMap.empty, Pattern {patternPre = PatternVar v,
                                       patternTy = ty})
  where
    getDataCtorMatching :: Monad m => TypeCheckT m VarName
    getDataCtorMatching = do
      cs <- getDataCtors
      case cs of
        Nothing -> return na0
        Just cs' ->
          case filter (isPrefixOf na0) (map varName cs') of
            [m] -> return m
            _ -> return na0

    getDataCtors :: Monad m => TypeCheckT m (Maybe [Var])
    getDataCtors = do
      rm <- Env.getRefMap
      case preTermCodRootType rm ty of
        Just (TermData d, _) -> fmap Just (Env.forceLookupDataCtor (varId d))
        _ -> return Nothing

    ctorFromVarStatus :: Monad m => VarStatus -> TypeCheckT m (Var, VarId, PreTerm)
    ctorFromVarStatus x = do
      x' <- varStatusTerm x
      case x' of
        Term {termPre = TermCtor v did, termTy = ty'} ->
          return (v, did, ty')
        _ -> error $ "expected var status to return ctor " ++ show x

    checkCtorType :: Monad m =>
      Var -> VarId -> PreTerm -> TypeCheckT m (SubstMap, Pattern)
    checkCtorType cv did cty = do
      r <- Env.getRefMap
      p <- makeImplicitApp cv did cty
      if not hasImplicitApp
         && isNothing (preTermDomCod r cty)
         && isNothing (preTermLazyCod r cty)
        then do
          su <- tcPatternUnify newpids lo ty (patternTy p)
          return (su, p)
        else return (IntMap.empty, p)

    makeImplicitApp :: Monad m =>
      Var -> VarId -> PreTerm -> TypeCheckT m Pattern
    makeImplicitApp cv did cty = do
      imps0 <- Env.forceLookupImplicit (varId cv)
      let ctor = PatternCtor cv did
      if null imps0
        then return (Pattern {patternPre = ctor, patternTy = cty})
        else do
          imps1 <- mapM getImplicit imps0
          let su = IntMap.fromList
                    (map (\(v0, x) ->
                      (varId v0, prePatternToPreTerm x)) imps1)
          let ias = map (\(v0, x) -> (varName v0, x)) imps1
          let a = PatternImplicitApp False ctor ias
          let aty = substPreTerm su cty
          return (Pattern {patternPre = a, patternTy = aty})

    getImplicit :: Monad m => (Var, PreTerm) -> TypeCheckT m (Var, PrePattern)
    getImplicit (v, _) = do
      i <- Env.freshVarId
      let v' = mkVar i ('.' : varName v)
      return (v, PatternVar v')
doTcPattern _ _ newpids ty (ParsePatternEmpty lo) = do
  verifyIsEmptyType lo newpids ty
  return (IntMap.empty, Pattern {patternPre = PatternEmpty,
                                 patternTy = ty})
doTcPattern _ _ newpids ty (ParsePatternUnit lo) = do
  su <- tcPatternUnify newpids lo ty TermUnitTy
  return (su, Pattern {patternPre = PatternUnit, patternTy = TermUnitTy})

tcPatternArgs :: Monad m =>
  VarId -> SubstMap -> SubstMap -> [(Int, (Maybe Var, PreTerm), ParsePattern)] ->
  TypeCheckT m (SubstMap, SubstMap, [(Int, (Maybe Var, Pattern))])
tcPatternArgs _ su asu [] = return (su, asu, [])
tcPatternArgs newpids su asu ((idx, (Nothing, p), e) : xs) = do
  (psu, e') <- tcPattern newpids (substPreTerm asu (substPreTerm su p)) e
  asu' <- tcMergePatUnifMaps newpids (patternLoc e) psu asu
  (tsu, bsu, es') <- tcPatternArgs newpids su asu' xs
  let r = e' {patternTy = p}
  return (tsu, bsu, (idx, (Nothing, r)) : es')
tcPatternArgs newpids su asu ((idx, (Just v, p), e) : xs) = do
  (psu, e') <- tcPattern newpids (substPreTerm asu (substPreTerm su p)) e
  asu' <- tcMergePatUnifMaps newpids (patternLoc e) psu asu
  let e'' = prePatternToPreTerm (patternPre e')
  let su' = IntMap.insert (varId v) e'' su
  (tsu, bsu, es') <- tcPatternArgs newpids su' asu' xs
  let r = e' {patternTy = p}
  return (tsu, bsu, (idx, (Just v, r)) : es')

patternPreCanApply :: PrePattern -> Bool
patternPreCanApply (PatternApp _ _) = True
patternPreCanApply (PatternLazyApp _) = True
patternPreCanApply (PatternCtor _ _) = True
patternPreCanApply (PatternImplicitApp _ _ _) = True
patternPreCanApply (PatternVar _) = False
patternPreCanApply PatternUnit = False
patternPreCanApply PatternEmpty = False

isEmptyType :: Monad m => VarId -> PreTerm -> TypeCheckT m Bool
isEmptyType newpids ty = do
  r <- Env.getRefMap
  let ty' = preTermNormalize r ty
  tcs <- getTypeCtors ty'
  case tcs of
    Just is -> foldlM (meetCtorId ty') True is
    Nothing -> return False
  where
    getTypeCtors :: Monad m => PreTerm -> TypeCheckT m (Maybe [Var])
    getTypeCtors (TermApp _ f _) = getTypeCtors f
    getTypeCtors (TermLazyApp _ f) = getTypeCtors f
    getTypeCtors (TermImplicitApp _ f _) = getTypeCtors f
    getTypeCtors (TermData v) =
      fmap Just (Env.forceLookupDataCtor (varId v))
    getTypeCtors _ = return Nothing

    meetCtorId :: Monad m => PreTerm -> Bool -> Var -> TypeCheckT m Bool
    meetCtorId _ False _ = return False
    meetCtorId ty' True i = do
      ctor <- Env.forceLookupRef (varId i)
      r <- Env.getRefMap
      let (cod, io) = preTermFinalCod r (termTy ctor)
      let !() = assert (not io) ()
      s <- runExceptT (patternUnify2 newpids ty' cod)
      case s of
        Left (UnifyAbsurd _) -> return True
        _ -> return False

verifyIsEmptyType :: Monad m => Loc -> VarId -> PreTerm -> TypeCheckT m ()
verifyIsEmptyType lo newpids ty = do
  e <- isEmptyType newpids ty
  when (not e) $ do
    r <- Env.getRefMap
    im <- Env.getImplicitMap
    err lo (Recoverable $
              "unable to match empty pattern with inferred type\n"
              ++ preTermToString im r defaultExprIndent ty)

dependencyOrderArgs ::
  (t -> VarIdSet) -> [(Maybe Var, t)] -> [a] ->
  [(Int, (Maybe Var, t), a)]
dependencyOrderArgs = \f ps es -> do
  let vs = paramVars ps
  let ps' = zipParamVars f 0 ps es
  doRearrange vs ps'
  where
    paramVars :: [(Maybe Var, t)] -> VarIdSet
    paramVars [] = IntSet.empty
    paramVars ((Nothing, _) : ps) = paramVars ps
    paramVars ((Just v, _) : ps) = IntSet.insert (varId v) (paramVars ps)

    zipParamVars ::
      (t -> VarIdSet) -> Int -> [(Maybe Var, t)] -> [a] ->
      [(Int, (Maybe Var, t, VarIdSet), a)]
    zipParamVars _ _ [] [] = []
    zipParamVars f idx ((v, p) : ps) (e : es) =
      (idx, (v, p, f p), e) : zipParamVars f (idx + 1) ps es
    zipParamVars _ _ _ _ = error "unreachable case"

    doRearrange :: VarIdSet ->
                   [(Int, (Maybe Var, t, VarIdSet), a)] ->
                   [(Int, (Maybe Var, t), a)]
    doRearrange _ [] = []
    doRearrange vs ps =
      let (vs', n, ps') = findNextFreeParam vs ps
      in n : doRearrange vs' ps'

    findNextFreeParam ::
      VarIdSet -> [(Int, (Maybe Var, t, VarIdSet), a)] ->
      (VarIdSet, (Int, (Maybe Var, t), a),
        [(Int, (Maybe Var, t, VarIdSet), a)])
    findNextFreeParam _ [] = error "is there a cyclic parm dependency?"
    findNextFreeParam vs (x@(idx, (v, p, is), e) : ps) =
      if IntSet.disjoint vs is
        then
          case v of
            Nothing -> (vs, (idx, (v, p), e), ps)
            Just v' -> (IntSet.delete (varId v') vs, (idx, (v, p), e), ps)
        else
          let (vs', n, ps') = findNextFreeParam vs ps
          in (vs', n, x : ps')
