{-# LANGUAGE BangPatterns #-}

module TypeCheck.TypeCheck (runTT)
where

import Str
import Loc (Loc)
import PackageMap (PackageMap, takePackage)
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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import TypeCheck.SubstMap
import Data.List (find, sortOn, sortBy, partition, isPrefixOf)
import Data.Foldable (foldlM, foldrM)
import System.Directory
  (canonicalizePath, doesFileExist, getPermissions, readable)
import Control.Exception (assert)
import TypeCheck.TypeCheckIO
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
  Bool -> Bool -> PackageMap -> FilePath -> String -> Program ->
  IO (Either String ([RefVar], DataCtorMap, ImplicitMap, RefMap))
runTypeCheckT verbose compiling packagePaths file moduleName prog = do
  r <- runExceptT
        (runReaderT
          (evalStateT
            (Env.runEnvT (tcInitProgram compiling packagePaths prog))
              (Map.singleton file Map.empty)) (verbose, file, moduleName))
  case r of
    Left e -> return (Left (typeCheckErrMsg e))
    Right x -> return (Right x)

importFilePath ::
  PackageMap -> FilePath -> TypeCheckIO (Either String FilePath)
importFilePath packagePaths p =
  case takePackage p of
    Nothing ->
      return (Left $ "import " ++ quote p
                     ++ " is missing package prefix "
                     ++ quote (".../" ++ p))
    Just (pack, postfix) ->
      case Map.lookup pack packagePaths of
        Nothing ->
          return $ Left $
            "unknown package prefix " ++ quote pack
            ++ " in import " ++ quote p
        Just prefix ->
          fmap Right (liftIO $ canonicalizePath (prefix ++ postfix ++ ".yu"))

runTT ::
  Bool -> Bool -> PackageMap -> FilePath ->
  IO (Either String ([RefVar], DataCtorMap, ImplicitMap, RefMap))
runTT verbose compiling packagePaths file = do
  isf <- liftIO (doesFileExist file)
  if not isf
  then return (Left $ "unable to open file " ++ file)
  else do
    perms <- liftIO (getPermissions file)
    if not (readable perms)
    then return (Left $ "unable to read file " ++ file)
    else do
      prog <- runParse file
      case prog of
        Left e -> return (Left e)
        Right prog' -> do
          file' <- liftIO (canonicalizePath file)
          runTypeCheckT verbose compiling packagePaths file' "" prog'

varStatusTerm :: VarStatus -> TypeCheckIO Term
varStatusTerm (StatusTerm te) = return te
varStatusTerm (StatusUnknownCtor subst depth n d) =
  Env.withDepth depth (tcDataDefCtorLookup subst n d)
varStatusTerm (StatusUnknownRef subst depth d) =
  Env.withDepth depth (tcDef subst False d)
varStatusTerm (StatusUnknownVar subst depth (lo, na) i e) =
  case e of
    Nothing -> err lo $ Fatal ("cannot determine type of variable " ++ quote na)
    Just e' -> Env.withDepth depth (tcVar subst (lo, na) i e')
varStatusTerm (StatusInProgress _ (lo, na)) =
  err lo $ Fatal (quote na ++ " has cyclic type")

verifyMultipleUsesIdent :: Bool -> (Loc, VarName) -> TypeCheckIO ()
verifyMultipleUsesIdent True (lo, na) = 
  err lo (Fatal $ "multiple instances of identifier " ++ quote na ++ " in scope")
verifyMultipleUsesIdent False _ = return ()

insertUnknownCtor ::
  SubstMap -> (Loc, VarName) -> Decl -> TypeCheckIO ()
insertUnknownCtor subst da d = do
  let (dna, dop) = operandSplit (declName d)
  na <- updateOperandTypeString (declLoc d, dna) dop
  let lo = declLoc d
  checkRefNameValid False (lo, na)
  depth <- Env.getDepth
  x <- Env.tryInsert na (StatusUnknownCtor subst depth da d) (depth == 0)
  verifyMultipleUsesIdent (isJust x) (lo, na)

insertUnknownRef :: SubstMap -> Def -> TypeCheckIO ()
insertUnknownRef subst d = do
  let (dna, dop) = operandSplit (defName d)
  na <- updateOperandTypeString (defLoc d, dna) dop
  let lo = defLoc d
  checkRefNameValid False (lo, na)
  depth <- Env.getDepth
  x <- Env.tryInsert na (StatusUnknownRef subst depth d) (depth == 0)
  verifyMultipleUsesIdent (isJust x) (lo, na)

insertUnknownVar ::
  SubstMap -> (Loc, VarName) -> VarId -> Maybe Expr -> TypeCheckIO ()
insertUnknownVar subst (lo, na) i e = do
  depth <- Env.getDepth
  let s = StatusUnknownVar subst depth (lo, na) i e
  if na == "_"
    then
      return ()
    else do
      checkVarNameValid (lo, na)
      x <- Env.tryInsert na s False
      verifyMultipleUsesIdent (isJust x) (lo, na)

markInProgress :: Bool -> (Loc, VarName) -> TypeCheckIO (Maybe Term)
markInProgress isCtor (lo, na) = do
  x <- lookupEnv na
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

updateToStatusTerm :: VarName -> Term -> TypeCheckIO ()
updateToStatusTerm na te = do
  x <- Env.tryUpdateIf
        (\c -> isStatusInProgress c || isStatusTerm c) na (StatusTerm te)
  when (isNothing x) (error $ "expected in-progress or term env status of " ++ quote na)

insertNonblankFreshVariable :: (Loc, VarName) -> PreTerm -> TypeCheckIO Var
insertNonblankFreshVariable na te = do
  i <- Env.freshVarId
  insertNonblankVariable na i te

insertNonblankVariable :: (Loc, VarName) -> VarId -> PreTerm -> TypeCheckIO Var
insertNonblankVariable (lo, na) i te = do
  let v = mkVar i na
  let v' = TermVar False v
  let y = StatusTerm (mkTerm v' te False)
  if na == "_"
    then
      return v
    else do
      checkVarNameValid (lo, na)
      x <- Env.tryInsert na y False
      verifyMultipleUsesIdent (isJust x) (lo, na)
      return v

checkVarNameValid :: (Loc, VarName) -> TypeCheckIO ()
checkVarNameValid (lo, na) = do
  when (isKeyword na)
          (err lo (Fatal $
                    "invalid variable name " ++ quote na
                    ++ ", it is a keyword"))
  when invalidName (err lo (Fatal $ "invalid variable name " ++ quote na))
  where
    invalidName :: Bool
    invalidName = '.' `elem` na || '#' `elem` na

checkRefNameValid :: Bool -> (Loc, VarName) -> TypeCheckIO ()
checkRefNameValid allowDot (lo, na0) = do
  when (isStrictKeyword (head nas))
          (err lo (Fatal $
                    "invalid identifier " ++ quote na0
                    ++ ", it is a strict keyword"))
  let le = length nas
  let (fnas, [lnas]) = splitAt (le - 1) nas
  let b = if le > 1 && lnas == ""
          then any invalidName fnas
          else any invalidName nas
  when b (err lo (Fatal $ "invalid name " ++ quote na0))
  where
    nas :: [VarName]
    nas = splitOn '#' na0

    splitOn :: Char -> VarName -> [VarName]
    splitOn _ "" = [""]
    splitOn c (x : xs) =
      let s = splitOn c xs
      in if c == x
          then "" : s
          else (x : head s) : tail s

    invalidName :: VarName -> Bool
    invalidName n = null n || hasInvalidDot || n == "_"

    hasInvalidDot :: Bool
    hasInvalidDot =
      not allowDot && elem '.' (drop 1 (takeWhile (/= '#') na0))

checkMainExists :: TypeCheckIO ()
checkMainExists = do
  ma <- lookupEnv "main"
  case ma of
    Nothing ->
      err (Loc.loc 1 1) (Fatal $ "missing " ++ quote "main" ++ " function")
    Just (StatusTerm t) -> do
      rm <- Env.getRefMap
      case preTermNormalize rm (termTy t) of
        TermArrow True [(Nothing, TermUnitTy)] TermUnitTy -> checkBody (termPre t)
        _ -> do
          ss <- preTermToString defaultExprIndent (TermArrow True [(Nothing, TermUnitTy)] TermUnitTy)
          err (Loc.loc 1 1)
            (Fatal $
              "expected type of " ++ quote "main" ++ " to be\n" ++ ss)
    Just _ -> error "unexpected status of main function"
  where
    checkBody :: PreTerm -> TypeCheckIO ()
    checkBody (TermRef v _) = do
      r <- Env.forceLookupRef (varId v)
      case termPre r of
        TermFun _ _ _ _ -> return ()
        _ -> invalidMainBody
    checkBody _ = invalidMainBody

    invalidMainBody :: TypeCheckIO ()
    invalidMainBody =
      err (Loc.loc 1 1)
        (Fatal $
          "it is required that " ++ quote "main"
          ++ " is defined by let () => ...")

tcInitProgram ::
  Bool -> PackageMap -> Program -> TypeCheckIO [RefVar]
tcInitProgram compiling packagePaths prog = doTcInitProgram
  where
    doTcInitProgram :: TypeCheckIO [RefVar]
    doTcInitProgram = do
      rs <- tcProgram packagePaths prog
      mapM_ checkPositivity rs
      rm <- Env.getRefMap
      let ks = IntMap.keys rm
      mapM_ verifyRecursiveImpureChains ks
      mapM_ verifyDataImpureChains rs
      mapM_ verifyImpureIsNotTerminationChecked ks
      when compiling checkMainExists
      return rs

    checkPositivity :: RefVar -> TypeCheckIO ()
    checkPositivity (RefData dv) = do
      ctors <- Env.forceLookupDataCtor (varId dv)
      mapM_ doCheck ctors
      where
        doCheck :: Var -> TypeCheckIO ()
        doCheck cv = do
          meta <- Env.forceLookupRefMeta (varId cv)
          imps <- Env.forceLookupImplicit (varId cv)
          c <- Env.forceLookupRef (varId cv)
          strictPositivityCheck (refMetaLoc meta) dv imps (termTy c)
    checkPositivity _ = return ()

insertAllImportExport ::
  Loc -> Bool -> [(VarName, (VarStatus, Bool))] -> TypeCheckIO ()
insertAllImportExport lo _ [] =
  err lo (Fatal $ "unable to find anything to import here")
insertAllImportExport lo b imex = doInsertAllImportExport lo b imex

doInsertAllImportExport ::
  Loc -> Bool -> [(VarName, (VarStatus, Bool))] -> TypeCheckIO ()
doInsertAllImportExport _ _ [] = return ()
doInsertAllImportExport lo b ((_, (_, False)) : imex) =
  doInsertAllImportExport lo b imex
doInsertAllImportExport lo b ((na, (t, True)) : imex) = do
  x <- Env.tryInsert na t b
  rm <- Env.getRefMap
  let multi = case x of
                Nothing -> False
                Just (StatusTerm x') ->
                  case t of
                    StatusTerm t' ->
                      not (preTermsAlphaEqual rm (termPre t') (termPre x'))
                    _ -> True
                _ -> True
  verifyMultipleUsesIdent multi (lo, na)
  doInsertAllImportExport lo b imex

insertImportExport ::
  Set VarName -> VarName -> Env.ScopeMap -> [(Loc, VarName, Bool)] ->
  TypeCheckIO ()
insertImportExport _ _ _ [] = return ()
insertImportExport vis ina em (ii@(lo, dna@('.' : '.' : '.' : '#' : na0), b) : imex) = do
  when (null na0) (err lo $ Fatal $ "operand type cannot be empty here")
  if isKeyOperandType na0
  then do
    em' <- filterM (isOperatorOf na0) (Map.toList em)
    insertAllImportExport lo b em'
    insertImportExport Set.empty ina em imex
  else do
    t <- lookupEnv na0
    case t of
      Nothing -> do
        when (Set.member dna vis) $
          err lo (Fatal $ "unknown operand type " ++ quote na0)
        insertImportExport (Set.insert dna vis) ina em (imex ++ [ii])
      Just t1 ->
        case t1 of
          StatusTerm t2 ->
            case termPre t2 of
              TermData v -> do
                em' <- filterM (isOperatorOf (varName v)) (Map.toList em)
                insertAllImportExport lo b em'
                insertImportExport Set.empty ina em imex
              _ -> err lo (Fatal $ "unexpected operand type "
                                   ++ quote na0 ++ ", not a data type")
          _ -> error "expected status term for import/export"
  where
    isOperatorOf ::
      VarName -> (VarName, (VarStatus, Bool)) -> TypeCheckIO Bool
    isOperatorOf v (vn, _) =
      let (_, vtn) = operandSplit vn
      in return (Just v == vtn)
insertImportExport _ ina em ((lo, ('.' : '.' : '.' : []), b) : imex) = do
  insertAllImportExport lo b (Map.toList em)
  insertImportExport Set.empty ina em imex
insertImportExport vis ina em (ii@(lo, na0, b) : imex) = do
  checkRefNameValid False (lo, na0)
  let (na1, opty) = operandSplit na0
  na <- updateOperandTypeString (lo, na1) opty
  case Map.lookup na em of
    Nothing -> do
      when (Set.member na0 vis) $
        err lo (Fatal $ "unable to find " ++ quote na
                        ++ " in module " ++ quote ina)
      insertImportExport (Set.insert na0 vis) ina em (imex ++ [ii])
    Just (StatusTerm tm, True) -> do
      x <- Env.tryInsert na (StatusTerm tm) b
      verifyMultipleUsesIdent (isJust x) (lo, na)
      insertImportExport Set.empty ina em imex
    Just (_, c) -> do
      let !() = assert (not c) ()
      err lo (Fatal $ "name " ++ quote na
                      ++ " is not exported by module " ++ quote ina)

tcProgram :: PackageMap -> Program -> TypeCheckIO [RefVar]
tcProgram _ ([], defs) = do
  rs <- tcDefsToVars IntMap.empty defs
  Env.clearImplicitVarMap
  return rs
tcProgram packagePaths ((malias, (lo, ina), imex) : imps, defs) = do
  case malias of
    Nothing -> return ()
    Just (alo, alias) -> do
      checkVarNameValid (alo, alias)
      canInsert <- Env.tryInsertModuleExpand alias ina
      when (not canInsert)
        (err alo (Fatal $ "multiple module aliases " ++ quote alias))
  (verbose, _, _) <- ask
  inaPath0 <- importFilePath packagePaths ina
  case inaPath0 of
    Left e0 -> err lo (Fatal e0)
    Right inaPath -> do
      isf <- liftIO (doesFileExist inaPath)
      when (not isf)
        (err lo (Fatal $ "unable to find import file \"" ++ inaPath ++ "\""))
      perms <- liftIO (getPermissions inaPath)
      when (not (readable perms))
        (err lo (Fatal $ "unable to read file \"" ++ inaPath ++ "\""))
      is <- lift get
      case Map.lookup inaPath is of
        Just em -> do
          insertImportExport Set.empty ina em imex
          tcProgram packagePaths (imps, defs)
        Nothing -> do
          lift (put (Map.insert inaPath Map.empty is))
          prog <- runParse inaPath
          case prog of
            Left e -> throwError (Fatal e)
            Right prog' -> do
              (em, vs) <- local (const (verbose, inaPath, ina)) $
                          Env.localEnv $ do
                            r <- tcProgram packagePaths prog'
                            Env.addToGlobals ina
                            em <- Env.getScopeMap
                            return (em, r)
              is' <- lift get
              lift (put (Map.insert inaPath em is'))
              insertImportExport Set.empty ina em imex
              fmap (++vs) (tcProgram packagePaths (imps, defs))

tcDefsToVars :: SubstMap -> [Def] -> TypeCheckIO [RefVar]
tcDefsToVars subst defs = do
  ts <- tcDefs subst defs
  mapM extractDefVar ts
  where
    extractDefVar :: Term -> TypeCheckIO RefVar
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
      ss <- preTermToString defaultExprIndent (termPre t)
      error $ "unexpected def term: " ++ ss

insertUnknownDataDefs ::
  Set VarName -> SubstMap -> [Def] -> TypeCheckIO ()
insertUnknownDataDefs _ _ [] = return ()
insertUnknownDataDefs vis subst (d : ds) = do
  b <- tryInsert `catchError` \ e -> do
        when (Set.member (defName d) vis) (throwError e)
        return False
  if b
  then insertUnknownDataDefs Set.empty subst ds
  else insertUnknownDataDefs (Set.insert (defName d) vis) subst (ds ++ [d])
  where
    tryInsert :: TypeCheckIO Bool
    tryInsert = do
      insertUnknownRef subst d
      return True

tcDefs :: SubstMap -> [Def] -> TypeCheckIO [Term]
tcDefs subst defs = do
  let (dataDefs, otherDefs) = partition isDataDef defs
  insertUnknownDataDefs Set.empty subst dataDefs
  mapM_ insertCtors dataDefs
  mapM_ (insertUnknownRef subst) otherDefs
  mapM (tcDef subst True) (sortBy valIsLT defs)
  where
    isDataDef :: Def -> Bool
    isDataDef (DefData _ _ _) = True
    isDataDef _ = False

    insertCtors :: Def -> TypeCheckIO ()
    insertCtors (DefData _ d cs) =
      mapM_ (insertUnknownCtor subst (declLoc d, declName d)) cs
    insertCtors _ = error "expected DefData for inserting ctors"

    valIsLT :: Def -> Def -> Ordering
    valIsLT (DefVal _ _ _) (DefVal _ _ _) = EQ
    valIsLT (DefVal _ _ _) _ = LT
    valIsLT _ (DefVal _ _ _) = GT
    valIsLT _ _ = EQ

typeOperandName :: PreTerm -> TypeCheckIO (Maybe VarName)
typeOperandName aty = do
  rm <- Env.getRefMap
  let aty' = preTermNormalize rm aty
  case preTermCodRootType rm aty' of
    Just (TermData v, _) -> return (Just (varName v))
    Just (TermTy, _) -> return (Just "Ty")
    _ -> return Nothing

tcDef :: SubstMap -> Bool -> Def -> TypeCheckIO Term
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
      fullEname <- fullRefName (declLoc d) (declName d)
      let rv = mkVar i fullEname
      Env.forceInsertImplicit i []
      let r = mkTerm (TermRef rv IntMap.empty) (termPre ty) False
      let (stna0, stopt) = operandSplit (declName d)
      stna <- updateOperandTypeString (declLoc d, stna0) stopt
      updateToStatusTerm stna r
      Env.forceInsertExtern i
      return r
tcDef subst _ (DefVal isPure d lets) = do
  ty1 <- Env.scope (tcDecl subst False d)
  case ty1 of
    Left term -> return term
    Right (imps, ty) -> do
      i <- Env.freshRefId
      fullVname <- fullRefName (declLoc d) (declName d)
      let rv = mkVar i fullVname
      Env.forceInsertImplicit i imps
      let r = mkTerm (TermRef rv IntMap.empty) (termPre ty) False
      let (stna0, stopt) = operandSplit (declName d)
      stna <- updateOperandTypeString (declLoc d, stna0) stopt
      updateToStatusTerm stna r
      e <- tcValCases subst stna imps (termPre ty) lets
      Env.forceInsertRef (declLoc d) fullVname isPure i e
      terminationCheck (declLoc d) rv
      return r
tcDef subst True (DefData isPure d ctors) = do
  depth <- Env.getDepth
  when (depth > 0)
    (err (declLoc d)
      (Fatal $ "nested " ++ quote "data" ++ " type in "
               ++ quote "where" ++ " clause is not supported"))
  tcDataAndCtors subst isPure d ctors
tcDef subst False def@(DefData isPure d _) = do
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
      fullDname <- fullRefName (declLoc d) (declName d)
      let v = mkVar i fullDname
      let r = mkTerm (TermData v) (termPre ty) False
      let (stna0, stopt) = operandSplit (declName d)
      stna <- updateOperandTypeString (declLoc d, stna0) stopt
      updateToStatusTerm stna r
      Env.forceInsertImplicit i imps
      Env.forceInsertRef (declLoc d) fullDname isPure i r
      Env.forceInsertDataCtor i []
      Env.forceInsertUnfinishedData i depth subst def
      return r

fullRefName :: Loc -> VarName -> TypeCheckIO VarName
fullRefName lo na = do
  let (na0, opty) = operandSplit na
  (_, _, modName) <- ask
  if modName /= ""
  then updateOperandTypeString (lo, na0 ++ "." ++ modName) opty
  else updateOperandTypeString (lo, na0) opty

tcDataAndCtors ::
  SubstMap -> Bool -> Decl -> [Decl] -> TypeCheckIO Term
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
          cis <- mapM (takeCtorFormVarList cis') ctors
          Env.removeUnfinishedData (varId v)
          Env.forceInsertDataCtor (varId v) cis
          let (stna0, stopt) = operandSplit (declName d)
          stna <- updateOperandTypeString (declLoc d, stna0) stopt
          updateToStatusTerm stna dt
          mapM_ (tcDataDefCtor subst v) (zip cis ctors)
          Env.forceInsertRef (declLoc d) (varName v) isPure (varId v) dt
          return dt
        _ -> do
          ss <- preTermToString 0 (termPre dt)
          error $ "expected " ++ ss ++ " to be a data type"
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
      fullDname <- fullRefName (declLoc d) (declName d)
      let v = mkVar i fullDname
      cis <- mapM (\x ->
                    Env.freshRefId >>= \j -> do
                      fullCname <- fullRefName (declLoc x) (declName x)
                      return $ mkVar j fullCname) ctors
      let r = mkTerm (TermData v) (termPre ty) False
      Env.forceInsertDataCtor i cis
      let (stna0, stopt) = operandSplit (declName d)
      stna <- updateOperandTypeString (declLoc d, stna0) stopt
      updateToStatusTerm stna r
      Env.forceInsertImplicit i imps
      Env.forceInsertRef (declLoc d) fullDname isPure i r
      mapM_ (tcDataDefCtor subst v) (zip cis ctors)
      Env.removeUnfinishedData i
      return r
  where
    addVarNotIn :: Set VarName -> Decl -> [Var] -> TypeCheckIO [Var]
    addVarNotIn s x vs = do
      fullCname <- fullRefName (declLoc x) (declName x)
      if Set.member fullCname s
      then return vs
      else do
        j <- Env.freshRefId
        return (mkVar j fullCname : vs)

    takeCtorFormVarList :: [Var] -> Decl -> TypeCheckIO Var
    takeCtorFormVarList [] c = do
      fullCname <- fullRefName (declLoc c) (declName c)
      error ("unable to find ctor " ++ fullCname ++ " in var list")
    takeCtorFormVarList (v : vs) c = do
      fullCname <- fullRefName (declLoc c) (declName c)
      if varName v == fullCname
      then return v
      else takeCtorFormVarList vs c

tcDataDefCtor ::
  SubstMap -> Var -> (Var, Decl) -> TypeCheckIO Term
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
      let (stna0, stopt) = operandSplit (declName ctor)
      stna <- updateOperandTypeString (declLoc ctor, stna0) stopt
      updateToStatusTerm stna r
      Env.forceInsertImplicit (varId cva) imps
      Env.forceInsertRef (declLoc ctor) (varName cva) True (varId cva) r
      return r
  where
    badCtorType =
      err (declLoc ctor) (Recoverable $
        "constructor needs to have " ++
        "codomain root type given by data " ++ quote (varName v))
    hasIoErr =
      err (declLoc ctor)
        (Fatal "constructor cannot have effectful function/lazy type")

verifyNoIoEscapeFromType :: Loc -> VarName -> Term -> TypeCheckIO ()
verifyNoIoEscapeFromType lo na t =
  when (termIo t) (err lo (Fatal $ "effect is escaping from type of " ++ quote na))

tcDecl ::
  SubstMap -> Bool -> Decl -> TypeCheckIO (Either Term (Implicits, Term))
tcDecl subst isCtor (Decl (lo, na) impls ty) =
  doTcDecl subst isCtor (Decl (lo, na) impls ty) `catchError`
    (\e -> case e of
            Recoverable m -> throwError (Fatal m)
            Fatal m -> throwError (Fatal m))

doTcDecl ::
  SubstMap -> Bool -> Decl -> TypeCheckIO (Either Term (Implicits, Term))
doTcDecl subst isCtor (Decl (lo, prena) impls ty) = do
  let (na0, opty) = operandSplit prena
  na <- updateOperandTypeString (lo, na0) opty
  t <- markInProgress isCtor (lo, na)
  case t of
    Nothing -> do
      mapM_ insertUnknownImplicit impls
      impls' <- mapM checkImplicit impls
      ty' <- evalTcExpr subst TermTy ty
      verifyNoIoEscapeFromType lo prena ty'
      verifyOperandArgument na (termPre ty')
      return (Right (impls', ty'))
    Just t' ->
      return (Left t')
  where
    verifyOperandArgument :: VarName -> PreTerm -> TypeCheckIO ()
    verifyOperandArgument na dty =
      if not (isOperator na)
      then return ()
      else do
        rm <- Env.getRefMap
        if isFixOp na
        then
          case preTermDomCod rm dty of
            Just ([(_, _)], _, _) -> return ()
            Just ([(_, _), (_, _)], _, _) -> return ()
            _ ->
              err lo $ Fatal $
                "expected exactly one/two arguments for prefix/infix operator"
        else -- postfix operator
          case preTermDomCod rm dty of
            Just ((_, _) : _, _, _) -> return ()
            _ ->
              err lo $ Fatal $
                "expected at least one argument for postix operator"

    insertUnknownImplicit :: VarListElem -> TypeCheckIO ()
    insertUnknownImplicit (_, Nothing) =
      error "expected implicit argument to have a type spec (@1)"
    insertUnknownImplicit ((vlo, vna), Just e) = do
      checkVarNameValid (vlo, vna)
      i <- Env.freshVarId
      insertUnknownVar subst (vlo, vna) i (Just e)

    checkImplicit ::
      VarListElem -> TypeCheckIO (Var, PreTerm)
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

isKeyOperandType :: String -> Bool
isKeyOperandType "Ty" = True
isKeyOperandType _ = False

updateOperandTypeString :: (Loc, VarName) -> Maybe VarName -> TypeCheckIO VarName
updateOperandTypeString (nlo, na) Nothing = do
  when (isOperator na)
    (err nlo (Fatal $ "missing operand type argument "
                      ++ quote "#..."))
  return na
updateOperandTypeString (nlo, na) (Just ss0) = do
  when (not (isOperator na))
    (err nlo (Fatal $ "operand type argument " ++ quote "#..."
                      ++ " is valid only for operators"))
  if (not (null ss0) && not (isKeyOperandType ss0))
  then do
    ss <- expandVarNameEnv ss0
    st <- lookupEnv ss
    case st of
      Nothing ->
        err nlo (Fatal $ "cannot find such operand type " ++ quote ss0) 
      Just st' -> do
        case st' of
          StatusTerm vt ->
            case termPre vt of
              TermData d -> return (na ++ "#" ++ varName d)
              _ -> errorNotData
          StatusUnknownRef _ _ _ -> addCurrentModule ss
          StatusInProgress _ _ -> addCurrentModule ss
          StatusUnknownCtor _ _ _ _ -> errorNotData
          StatusUnknownVar _ _ _ _ _ -> errorNotData
  else
    return (na ++ "#" ++ ss0)
  where
    errorNotData :: TypeCheckIO a
    errorNotData =
      err nlo $ Fatal $
        "unexpected operand type " ++ quote ss0 ++ ", not a data type"

    addCurrentModule :: VarName -> TypeCheckIO VarName
    addCurrentModule ss = do
      let (s0, s1) = operandSplit ss
      (_, _, modName) <- ask
      let s0' = case modName of
                  "" -> s0
                  _ -> s0 ++ "." ++ modName
      let ss' = operandConcatMaybe s0' s1
      return (na ++ "#" ++ ss')

tcVar :: SubstMap -> (Loc, VarName) -> VarId -> Expr -> TypeCheckIO Term
tcVar subst (lo, na) i e = do
  t <- markInProgress False (lo, na)
  case t of
    Nothing -> do
      ty <- evalTcExpr subst TermTy e
      verifyNoIoEscapeFromType lo na ty
      let v = mkTerm (TermVar False (mkVar i na)) (termPre ty) False
      updateToStatusTerm na v
      return v
    Just t' -> return t'

tcDataDefCtorLookup ::
  SubstMap -> (Loc, VarName) -> Decl -> TypeCheckIO Term
tcDataDefCtorLookup subst (dlo, dna) d = do
  x <- lookupEnv dna
  dt <- case x of
          Nothing ->
            error ("cannot find data type " ++ quote dna
                   ++ " for ctor " ++ quote (declName d))
          Just (StatusUnknownCtor _ _ _ _) ->
            error ("ctor " ++ quote dna
                   ++ " was expected to be data type")
          Just (StatusUnknownVar _ _ _ _ _) ->
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
      ctor <- do
        let (stna0, stopt) = operandSplit (declName d)
        stna <- updateOperandTypeString (declLoc d, stna0) stopt
        lookupEnv stna
      case ctor of
        Just (StatusTerm ctor') ->
          return ctor'
        Just (StatusInProgress _ _) ->
          err dlo (Fatal $
                    "constructor " ++ quote (declName d)
                    ++ " has cyclic type")
        Just (StatusUnknownCtor _ _ _ _) -> do
          eis <- Env.forceLookupDataCtor (varId v)
          fullCname <- fullRefName (declLoc d) (declName d)
          case find (\ei -> varName ei == fullCname) eis of
            Nothing -> do
              i <- Env.freshRefId
              let cv = mkVar i fullCname
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

tcValCases ::
  SubstMap -> VarName -> Implicits -> PreTerm -> [ValCase] -> TypeCheckIO Term
tcValCases subst na [] ty [ValCase lo' [] Nothing (Just (e, w))] = do
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
tcValCases _ na [] _ (ValCase lo' [] Nothing Nothing : _) = do
  err lo' (Fatal $ "case for " ++ quote na ++ " is not absurd")
tcValCases _ _ [] _ (ValCase _ [] Nothing (Just _) : ValCase lo' _ _ _ : _) = do
  err lo' (Fatal "case is unreachable")
tcValCases _ na [] _ (ValCase lo' (_:_) Nothing _ : _) = do
  err lo' (Fatal $ "cannot apply " ++ quote na ++ " to implicit arguments")
tcValCases subst na ips@(_:_) ty (le@(ValCase lo' _ Nothing _) : lets) = do
  (ct, io) <- buildValCaseTree na subst ips Nothing ty (le : lets)
  when io (err lo' (Fatal $ "effect escapes from " ++ quote na))
  let ins = map (varName . fst) ips
  let fn = TermFun ins False Nothing ct
  return (mkTerm fn ty False)
tcValCases subst na ips ty (le@(ValCase lo' _ (Just _) _) : lets) = do
  r <- Env.getRefMap
  case preTermDomCod r ty of
    Nothing -> err lo' (Recoverable $
                          "application of " ++ quote na
                          ++ ", but does not have function type")
    Just (dom, cod, io) -> do
      (ct, hasIo) <- buildValCaseTree na subst ips (Just dom) cod (le : lets)
      let ins = map (varName . fst) ips
      when (hasIo && not io)
        (err lo' (
          Recoverable $
            "expected type of " ++ quote na
            ++ " to be effectful " ++ quote "->>"))
      return (mkTerm (TermFun ins hasIo (Just (length dom)) ct) ty False)
tcValCases _ na _ _ [] = error ("missing let case for " ++ quote na)

type CasePatternTriple =
  ((Maybe Var, Bool), Pattern) -- Bool = True if forced.

buildValCaseTree ::
  VarName -> SubstMap -> Implicits -> Maybe [(Maybe Var, PreTerm)] -> PreTerm ->
  [ValCase] -> TypeCheckIO (CaseTree, Bool)
buildValCaseTree valName subst impParams domain codomain valCases = do
  iCaseData <- extractImplicitCaseData valCases
  r0 <- Env.getRefMap
  let iCaseData' = dependencyOrder r0 iCaseData
                    (map (\(v,x) -> (Just v, x)) impParams)
  caseData <- case domain of
                Nothing ->
                  verifyNoExplicitArgs valCases >> return []
                Just d -> do
                  cd <- extractExplicitCaseData (length d) valCases
                  r1 <- Env.getRefMap
                  return (dependencyOrder r1 cd d)
  let caseData' = if null caseData
                    then iCaseData'
                    else mergeCaseData (length impParams) iCaseData' caseData
  cs <- mapM checkCase caseData'
  casesToCaseTree cs
  where
    verifyNoExplicitArgs :: [ValCase] -> TypeCheckIO ()
    verifyNoExplicitArgs [] = return ()
    verifyNoExplicitArgs (ValCase lo _ (Just _) _ : _) =
      err lo (Recoverable $ "unexpected function application of " ++ quote valName)
    verifyNoExplicitArgs (ValCase _ _ Nothing _ : ls) =
      verifyNoExplicitArgs ls

    extractImplicitCaseData :: [ValCase] ->
      TypeCheckIO [([ParsePattern], Loc, Maybe (Expr, OptWhereClause))]
    extractImplicitCaseData [] = return []
    extractImplicitCaseData (ValCase lo ias _ e : ls) = do
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

    extractExplicitCaseData :: Int -> [ValCase] ->
      TypeCheckIO [([ParsePattern], Loc, Maybe (Expr, OptWhereClause))]
    extractExplicitCaseData _ [] = return []
    extractExplicitCaseData n (ValCase lo _ (Just ps) e : ls) = do
      when (length ps /= n)
        (err lo (Fatal $
          "expected " ++ show n ++ " argument(s), but case for "
          ++ quote valName ++ " is applied to " ++ show (length ps) ++ " argument(s)"))
      pss <- extractExplicitCaseData n ls
      return ((ps, lo, e) : pss)
    extractExplicitCaseData n (ValCase lo _ Nothing _ : _) =
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

    checkCase ::
      ([(Int, (Maybe Var, PreTerm), ParsePattern)], Loc, Maybe (Expr, OptWhereClause)) ->
      TypeCheckIO (Loc, [CasePatternTriple], Maybe Term)
    checkCase (args, lo, e) = doCheckCase args lo e

    doCheckCase ::
      [(Int, (Maybe Var, PreTerm), ParsePattern)] -> Loc ->
      Maybe (Expr, OptWhereClause) ->
      TypeCheckIO (Loc, [CasePatternTriple], Maybe Term)
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

    checkArgs ::
      [(Int, (Maybe Var, PreTerm), ParsePattern)] ->
      TypeCheckIO (SubstMap, SubstMap, [(Maybe Var, Pattern)])
    checkArgs as = do
      newpids <- Env.getNextVarId
      (m1, m2, ps) <- tcPatternArgs newpids IntMap.empty subst as
      return (m1, m2, map snd (sortOn fst ps))

tcUnfinishedData :: VarId -> TypeCheckIO ()
tcUnfinishedData dty = do
  dty' <- Env.lookupUnfinishedData dty
  case dty' of
    Nothing -> pure ()
    Just (depth, su, def) ->
      Env.withDepth depth (tcDef su True def >> pure ())

{-
showCasesToCaseTree ::
  ImplicitMap -> RefMap -> [(Loc, [CasePatternTriple], Maybe Term)] -> String
showCasesToCaseTree _ _ [] = ""
showCasesToCaseTree im rm ((_, ps, _) : pss) =
  let ss = map varPatToStr ps
  in intercalate ", " ss ++ "\n" ++ showCasesToCaseTree im rm pss
  where
    varPatToStr ((Nothing, b), p) =
      prePatternToString 0 (patternPre p)
      ++ " : " ++ preTermToString 0 (patternTy p)
      ++ " {forced? " ++ show b ++ "}"
    varPatToStr ((Just v, b), p) =
      varName v ++ "." ++ show (varId v) ++ " := " ++ varPatToStr ((Nothing, b), p)
-}

-- Bool = True if there is a leaf which is effectful.
casesToCaseTree ::
  [(Loc, [CasePatternTriple], Maybe Term)] -> TypeCheckIO (CaseTree, Bool)
casesToCaseTree pss = do
  x <- runExceptT (doCasesToCaseTree 0 pss)
  case x of
    Left lo -> err lo (Fatal "non-exhaustive patterns")
    Right (t, reachedCases, io) -> do
      let allCases = projectLocs pss
      let diff = Set.difference allCases reachedCases
      case Set.elems diff of
        [] -> return (t, io)
        c:_ -> err c (Fatal "case is unreachable")
  where
    projectLocs :: [(Loc, a, b)] -> Set Loc
    projectLocs [] = Set.empty
    projectLocs ((lo, _, _) : los) =
      Set.insert lo (projectLocs los)

doCasesToCaseTree ::
  Int -> [(Loc, [CasePatternTriple], Maybe Term)] ->
  ExceptT Loc TypeCheckIO (CaseTree, Set Loc, Bool)
doCasesToCaseTree _ [] = error "no cases to build case tree"
doCasesToCaseTree startIdx pss@((firstLoc, ps, te) : _) = do
  --rr <- lift Env.getRefMap
  --ii <- lift Env.getImplicitMap
  --let !() = trace (showCasesToCaseTree ii rr pss) ()
  midx <- lift (findCaseIndex ps)
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
          return (CaseEmpty idx, Set.singleton firstLoc, False)
        Nothing ->
          case te of
            Nothing -> lift . err firstLoc $ Fatal "missing case(s)"
            Just te' -> do
              let t = CaseLeaf vis (termPre te') (termNestedDefs te')
              return (t, Set.singleton firstLoc, termIo te')
    Right (idx, dty) -> do
      lift (tcUnfinishedData dty)
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
          let (subc, subcLoc, cio) = IntMap.foldlWithKey
                                          mergeLocs
                                          (IntMap.empty, Set.empty, False)
                                          subc0
          (subd, subdLoc, dio) <- case ca of
                                    Nothing ->
                                      return (Nothing, Set.empty, False)
                                    Just ca' -> do
                                      (d, lo, io) <- makeSubCase ca'
                                      return (Just d, lo, io)
          cnode <- lift $ makeCaseNode idx subc (ca, subd)
          return (cnode, Set.union subcLoc subdLoc, cio || dio)
  where
    mergeLocs ::
      (IntMap ([Var], CaseTree), Set Loc, Bool) ->
      Int -> (([Var], CaseTree), Set Loc, Bool) ->
      (IntMap ([Var], CaseTree), Set Loc, Bool)
    mergeLocs (m, s, io1) i (b, t, io2) =
      (IntMap.insert i b m, Set.union s t, io1 || io2)

    makeSubCase ::
      ([(Loc, [CasePatternTriple], Maybe Term)], [Var]) ->
      ExceptT Loc TypeCheckIO (([Var], CaseTree), Set Loc, Bool)
    makeSubCase (rss, ids) = do
      (tr, lo, io) <- doCasesToCaseTree 0 rss
      return ((ids, tr), lo, io)

    makeCaseNode ::
      Int -> IntMap ([Var], CaseTree) ->
        (Maybe ([(Loc, [CasePatternTriple], Maybe Term)], [Var]),
         Maybe ([Var], CaseTree)) ->
      TypeCheckIO CaseTree
    makeCaseNode idx subc (_, subd) =
      case IntMap.lookup (-1) subc of
        Nothing -> return (CaseNode idx subc subd)
        Just subc' -> return (CaseUnit idx subc')

    findCaseIndex ::
      [CasePatternTriple] -> TypeCheckIO (Either Bool (Int, VarId))
    findCaseIndex patterns = doFindCaseIndex startIdx (drop startIdx patterns)
      where
        doFindCaseIndex ::
          Int -> [CasePatternTriple] -> TypeCheckIO (Either Bool (Int, VarId))
        doFindCaseIndex _ [] =
          if startIdx == 0 then return (Left True) else return (Left False)
        doFindCaseIndex idx ((_, q) : qs) =
          case prePatternGetRoot (patternPre q) of
            PatternUnit -> return (Right (idx, -1))
            PatternCtor _ i -> do
              d <- hasLateDependency q qs
              if not d
              then return (Right (idx, i))
              else doFindCaseIndex (idx + 1) qs
            _ -> doFindCaseIndex (idx + 1) qs

        hasLateDependency ::
          Pattern -> [CasePatternTriple] -> TypeCheckIO Bool
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
    dataSplit ::
      VarId -> TypeCheckIO (IntMap PreTerm)
    dataSplit (-1) = return (IntMap.singleton (-1) TermUnitTy)
    dataSplit dv = do
      is <- Env.forceLookupDataCtor dv
      foldlM insm IntMap.empty is
      where
        insm :: IntMap PreTerm -> Var -> TypeCheckIO (IntMap PreTerm)
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

    duplicateCatchAll ::
      Int -> [VarId] -> [(Loc, [CasePatternTriple], Maybe Term)] ->
      TypeCheckIO [(Loc, [CasePatternTriple], Maybe Term, [Var])]
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

    testAllCtorsCovered ::
      Int -> CasePatternTriple -> IntSet -> TypeCheckIO Bool
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

    makeCtorPatterns ::
      (Maybe Var, Bool) -> [VarId] -> PreTerm -> TypeCheckIO [CasePatternTriple]
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
      --let !() = trace ("new ctor pattern " ++ prePatternToString 0 (patternPre c)) ()
      --let !() = trace ("unify pattern type " ++ preTermToString 0 (patternTy c) ++ " with " ++ preTermToString 0 ty) ()
      r <- runExceptT (patternUnify2 newpids ty (patternTy c))
      case r of
        Left (UnifyAbsurd _) -> do
          makeCtorPatterns b is ty
        _ -> do
          ps' <- makeCtorPatterns b is ty
          return ((b, c {patternTy = ty}) : ps')

    splitCtorCases ::
      Int -> [(Loc, [CasePatternTriple], Maybe Term, [Var])] ->
      TypeCheckIO (IntMap ([(Loc, [CasePatternTriple], Maybe Term)], [Var]))
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
          --let !() = trace ("unify " ++ preTermToString 0 (patternTy q) ++ " with " ++ preTermToString 0 (patternTy c')) ()
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

    reorderForceSubst ::
      VarId -> SubstMap -> [CasePatternTriple] -> TypeCheckIO SubstMap
    reorderForceSubst newpids su qs = do
      su' <- reorderForceSubstStep newpids su qs
      if IntMap.keys su /= IntMap.keys su'
      then reorderForceSubst newpids su' qs
      else return su'

    reorderForceSubstStep ::
      VarId -> SubstMap -> [CasePatternTriple] -> TypeCheckIO SubstMap
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

    pvarsImplicits ::
      [(VarName, Pattern)] -> TypeCheckIO [(VarName, Var)]
    pvarsImplicits qs =
      mapM (\(n, _) -> fmap (\i -> (n, mkVar i "_")) Env.freshVarId) qs

    pvarsArgs :: [Maybe [Pattern]] -> TypeCheckIO [Maybe [Var]]
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

    makeCatchAllCases ::
      Int -> [(Loc, [CasePatternTriple], Maybe Term, [Var])] ->
      TypeCheckIO (Maybe ([(Loc, [CasePatternTriple], Maybe Term)], [Var]))
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

verifyExprNoImplicits ::
  SubstMap -> IntMap (Loc, String) -> TypeCheckIO ()
verifyExprNoImplicits isu imps = do
  let imps' = IntMap.difference imps $ IntMap.restrictKeys imps (IntMap.keysSet isu)
  when (not (null imps')) $
    let imps'' = sortOn fst (map snd (IntMap.toList imps'))
    in mapM_ (\(lo, msg) -> err lo (Recoverable msg)) imps''

evalTcExpr :: SubstMap -> PreTerm -> Expr -> TypeCheckIO Term
evalTcExpr su ty e = do
  (e', (s1, imps)) <- runStateT (tcExpr su ty e)
                        (IntMap.empty, ImpMap IntMap.empty Nothing)
  case imps of
    ImpMap _ (Just _) -> error "unexpected parent ImpMap"
    ImpMap imps' Nothing ->
      verifyExprNoImplicits s1 imps' >> return e'

scope :: ExprIO Term -> ExprIO Term
scope e = fmap fst (dataScope (fmap (\e' -> (e', ())) e))

dataScope :: ExprIO (Term, a) -> ExprIO (Term, a)
dataScope e = do
  st <- get
  (x, st') <- lift (Env.scope (runStateT e st))
  put st'
  return x

forceIfLazyPreTerm :: PreTerm -> ExprIO PreTerm
forceIfLazyPreTerm t = do
  rm <- lift Env.getRefMap
  let n = preTermNormalize rm t
  case n of
    TermLazyArrow _ _ -> return (doForceIfLazyPreTerm n)
    _ -> return t

doForceIfLazyPreTerm :: PreTerm -> PreTerm
doForceIfLazyPreTerm (TermLazyArrow _ t) = doForceIfLazyPreTerm t
doForceIfLazyPreTerm t = t

forceIfLazyTerm :: Term -> ExprIO Term
forceIfLazyTerm t = do
  rm <- lift Env.getRefMap
  let n = preTermNormalize rm (termTy t)
  case n of
    TermLazyArrow _ _ ->
      doForceIfLazyTerm (termPre t) n (termIo t) (termNestedDefs t)
    _ -> return t

doForceIfLazyTerm ::
  PreTerm -> PreTerm -> Bool -> [RefVar] -> ExprIO Term
doForceIfLazyTerm te (TermLazyArrow io1 cod) io2 defs =
  doForceIfLazyTerm (TermLazyApp io1 te) cod (io1 || io2) defs
doForceIfLazyTerm te ty io defs =
  return $ Term { termPre = te
                , termTy = ty
                , termNestedDefs = defs
                , termIo = io }

tcExpr :: SubstMap -> PreTerm -> Expr -> ExprIO Term
tcExpr subst ty e = do
  rm <- lift Env.getRefMap
  case preTermLazyCod rm ty of
    Nothing -> exprCheck ty
    Just (cod, io) -> exprCheckLazy io cod
  where
    exprCheck :: PreTerm -> ExprIO Term
    exprCheck expectedTy = do
      e0 <- doTcExpr False subst Nothing (Just expectedTy) e
      e' <- forceIfLazyTerm e0
      tcExprSubstUnify (exprLoc e) expectedTy (termTy e')
      su <- getExprSubst
      let se = substPreTerm su (termPre e')
      let st = substPreTerm su expectedTy
      return (mkTerm se st (termIo e'))

    exprCheckLazy :: Bool -> PreTerm -> ExprIO Term
    exprCheckLazy io expectedTy = do
      e' <- tcExpr subst expectedTy e
      tcExprSubstUnify (exprLoc e) expectedTy (termTy e')
      su <- getExprSubst
      let se = TermLazyFun io (substPreTerm su (termPre e'))
      let st = TermLazyArrow io (substPreTerm su expectedTy)
      return (mkTerm se st False)

doTcExprSubst ::
  Bool -> SubstMap -> Maybe (Either Expr (Expr, Expr)) -> Expr -> ExprIO Term
doTcExprSubst isTrial subst operandArg e = do
  e' <- doTcExpr isTrial subst operandArg Nothing e
  isu <- getExprSubst
  return (mkTerm (substPreTerm isu (termPre e'))
                 (substPreTerm isu (termTy e'))
                 (termIo e'))

-- The first Bool is a simple heuristic. It is indicating whether
-- only the root type of the expression is needed. It is certainly
-- possible to do better, fx by adding this to tcExpr as well.
doTcExpr ::
  Bool -> SubstMap -> Maybe (Either Expr (Expr, Expr)) ->
  Maybe PreTerm -> Expr -> ExprIO Term
doTcExpr _ _ _ _ (ExprUnitElem _) = return mkTermUnitElem
doTcExpr _ _ _ _ (ExprUnitTy _) = return mkTermUnitTy
doTcExpr _ _ _ _ (ExprTy _) = return mkTermTy
doTcExpr _ subst operandArg ty (ExprVar (lo, na0)) = do
  lift (checkRefNameValid True (lo, na0))
  na <- operandVarName
  x <- lift (lookupEnv na)
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
    getOperatorContext ::
      TypeCheckIO
        (Maybe
          (Either
            (Either Expr PreTerm)
            (Either (Expr, Expr) (PreTerm, PreTerm))))
    getOperatorContext =
      case ty of
        Just ty' -> do
          rm <- Env.getRefMap
          case preTermDomCod rm ty' of
            Nothing -> return Nothing
            Just ([], _, _) -> return Nothing
            Just ([(_, t)], _, _) -> return (Just (Left (Right t)))
            Just ((_, t1) : (_, t2) : _, _, _) ->
              return (Just (Right (Right (t1, t2))))
        Nothing ->
          case operandArg of
            Nothing -> return Nothing
            Just (Left e) -> return (Just (Left (Left e)))
            Just (Right e) -> return (Just (Right (Left e)))

    operandVarName :: ExprIO VarName
    operandVarName =
      if not (isOperator na0)
      then return na0
      else
        if '#' `elem` na0
        then do
          let (na1, opty) = operandSplit na0
          na1' <- lift (expandVarNameEnv na1)
          lift (updateOperandTypeString (lo, na1') opty)
        else do
          ctx <- lift getOperatorContext
          case ctx of
            Nothing -> unableToInferOperand
            Just (Left u) -> unaryContext u
            Just (Right b) -> binaryContext b

    binaryContext ::
      Either (Expr, Expr) (PreTerm, PreTerm) -> ExprIO VarName
    binaryContext (Left (e1, e2)) =
        if isRightAssocFixOp na0
        then operandVarNameFromArg e2
        else -- postfix or left assoc:
          operandVarNameFromArg e1
    binaryContext (Right (t1, t2)) =
        if isRightAssocFixOp na0
        then operandVarNameFromType t2
        else -- postfix or left assoc:
          operandVarNameFromType t1

    unaryContext ::
      Either Expr PreTerm -> ExprIO VarName
    unaryContext (Left e1) = operandVarNameFromArg e1
    unaryContext (Right t1) = operandVarNameFromType t1

    checkOperand :: Expr -> ExprIO Term
    checkOperand a = do
      s <- get
      r <- lift $ evalStateT (doTcExprSubst True subst Nothing a) s
      return r

    operandVarNameFromArg :: Expr -> ExprIO VarName
    operandVarNameFromArg a = do
      a' <- checkOperand a
      operandVarNameFromType (termTy a')

    operandVarNameFromType :: PreTerm -> ExprIO VarName
    operandVarNameFromType aty = do
      --let !() = trace (preTermToString 0 aty) ()
      n <- lift (typeOperandName aty)
      case n of
        Nothing -> unableToInferOperand
        Just n' -> do
          na00 <- lift (expandVarNameEnv na0)
          let na0' = na00 ++ "#"
          let na' = operandConcat na00 n'
          x1 <- lift $ lookupEnv na0'
          x2 <- lift $ lookupEnv na'
          when (isJust x1 && isJust x2)
            (lift $ err lo (Fatal $
                             "multiple candidates for operator "
                             ++ quote na0
                             ++ ", use operand type argument "
                             ++ quote "#..."
                             ++ " to disambiguate"))
          if isJust x1
          then return na0'
          else return na'

    unableToInferOperand :: ExprIO VarName
    unableToInferOperand = do
      na00 <- lift (expandVarNameEnv na0)
      let na0' = na00 ++ "#"
      x <- lift $ lookupEnv na0'
      when (isNothing x)
        (lift $ err lo (Recoverable $
                  "unable to infer operand type of " ++ quote na0))
      return na0'

    makeImplicitApp :: Var -> Term -> ExprIO Term
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

    getImplicit :: (Var, PreTerm) -> ExprIO (Var, PreTerm)
    getImplicit (v, vt) = do
      i <- lift Env.freshVarId
      l <- lift Env.getNextLocalVarName
      let v' = mkVar i (varName v ++ "_" ++ l)
      impMapInsert i vt lo $
        "unable to infer implicit argument " ++ varName v
      return (v, TermVar True v')
doTcExpr isTrial subst _ ty (ExprFun lo as body) = do
  scope $ do
    as' <- mapM (\a -> fmap ((,) a) (lift Env.freshVarId)) as
    case ty of
      Nothing -> do
        implicits <- mapM insertImplicit as
        ats <- mapM insertUnknownUntypedParam (zip as' implicits)
        dom' <- mapM checkParam (zip as' ats)
        body' <- doTcExpr isTrial subst Nothing Nothing body
        let io = termIo body'
        let fte = TermFun [] io (Just (length dom'))
                    (CaseLeaf (map fst dom') (termPre body') [])
        let mdom = map (\(d1, d2) -> (Just d1, d2)) dom'
        let fty = TermArrow io mdom (termTy body')
        return (mkTerm fte fty False)
      Just ty' -> do
        rm <- lift Env.getRefMap
        case preTermDomCod rm ty' of
          Nothing -> do
            implicits <- mapM insertImplicit as
            ats <- mapM insertUnknownUntypedParam (zip as' implicits)
            dom' <- mapM checkParam (zip as' ats)
            body' <- doTcExpr isTrial subst Nothing Nothing body
            let io = termIo body'
            let fte = TermFun [] io (Just (length dom'))
                        (CaseLeaf (map fst dom') (termPre body') [])
            let mdom = map (\(d1, d2) -> (Just d1, d2)) dom'
            let fty = TermArrow io mdom (termTy body')
            tcExprSubstUnify lo ty' fty
            return (mkTerm fte fty False)
          Just (dom, cod, io) -> do
            when (length dom /= length as')
              (lift $ err lo (Fatal $
                               "expected " ++ show (length dom)
                               ++ " argument(s), but function has "
                               ++ show (length as')))
            mapM_ insertUnknownTypedParam as'
            rm0 <- lift Env.getRefMap
            let bs = dependencyOrderArgs (preTermVars rm0) dom as'
            (su0, dom0) <- insertTypedParams IntMap.empty bs
            let dom' = map snd (sortOn fst dom0)
            let cod' = substPreTerm su0 cod
            body' <- tcExpr subst cod' body
            let bodyIo = termIo body'
            when (bodyIo && not io)
              (lift $ err lo (Recoverable $
                  "type of function is effectful, but expected "
                  ++ "pure function type"))
            let fte = TermFun [] bodyIo (Just (length dom'))
                        (CaseLeaf (map fst dom') (termPre body') [])
            let mdom = map (\(d1, d2) -> (Just d1, d2)) dom'
            let fty = TermArrow io mdom cod'
            isu <- getExprSubst
            tcExprUnify lo ty' (substPreTerm isu fty)
            return (mkTerm fte fty False)
  where
    insertImplicit ::
      VarListElem -> ExprIO (Maybe PreTerm)
    insertImplicit ((vlo, vna), Nothing) = do
      i <- lift Env.freshVarId
      let e = TermVar True (mkVar i ("TypeOf_" ++ vna))
      impMapInsert i TermTy vlo $ "unable to infer type of " ++ quote vna
      return (Just e)
    insertImplicit (_, Just _) = return Nothing

    insertUnknownUntypedParam ::
      ((VarListElem, VarId), Maybe PreTerm) -> ExprIO (Maybe (Var, PreTerm))
    insertUnknownUntypedParam ((((vlo, vna), Nothing), i), Just e) = do
      v <- lift (insertNonblankVariable (vlo, vna) i e)
      return (Just (v, e))
    insertUnknownUntypedParam ((((vlo, vna), Just e), i), _) = do
      lift (insertUnknownVar subst (vlo, vna) i (Just e))
      return Nothing
    insertUnknownUntypedParam _ = error "unexpected case"

    checkParam ::
      ((VarListElem, VarId), Maybe (Var, PreTerm)) -> ExprIO (Var, PreTerm)
    checkParam (((_, Nothing), _), Nothing) =
      error "expected function explicit type or implicit type"
    checkParam (((_, Nothing), _), Just x) = return x
    checkParam ((((vlo, vna), Just e), i), _)
      | vna == "_" = do
          e' <- tcExpr subst TermTy e
          lift (verifyNoIoEscapeFromType vlo vna e')
          let v = mkVar i "_"
          return (v, termPre e')
      | True = do
          pa <- lift (markInProgress False (vlo, vna))
          case pa of
            Nothing -> do
              e' <- tcExpr subst TermTy e
              lift (verifyNoIoEscapeFromType vlo vna e')
              let v = mkVar i vna
              lift $ updateToStatusTerm vna
                        (mkTerm (TermVar False v) (termPre e') False)
              return (v, termPre e')
            Just (Term {termPre = TermVar _ v, termTy = tt}) ->
              return (v, tt)
            _ ->
              error $ "not a variable term " ++ quote vna

    insertUnknownTypedParam :: (VarListElem, VarId) -> ExprIO ()
    insertUnknownTypedParam (((vlo, vna), Nothing), i) =
      lift (insertUnknownVar subst (vlo, vna) i Nothing)
    insertUnknownTypedParam (((vlo, vna), Just e), i) =
      lift (insertUnknownVar subst (vlo, vna) i (Just e))

    insertTypedParams ::
      SubstMap -> [(Int, (Maybe Var, PreTerm), (VarListElem, VarId))] ->
      ExprIO (SubstMap, [(Int, (Var, PreTerm))])
    insertTypedParams su [] = return (su, [])
    insertTypedParams su ((idx, (v, t), (((vlo, vna), e), i)) : xs) = do
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
      let var = mkVar i vna
      let tvar = TermVar False var
      when (vna /= "_")
        (lift $ updateToStatusTerm vna (mkTerm tvar t' False))
      let su' = case v of
                  Nothing -> su
                  Just v' -> IntMap.insert (varId v') tvar su 
      (su'', ts) <- insertTypedParams su' xs
      return (su'', (idx, (var, t')) : ts)
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
    insertUnknownParam :: ExprListTypedElem -> ExprIO ()
    insertUnknownParam (Left _) = return ()
    insertUnknownParam (Right ((vlo, vna), e)) = do
      i <- lift Env.freshVarId
      lift (insertUnknownVar subst (vlo, vna) i (Just e))

    checkParam ::
      ExprListTypedElem -> ExprIO (Maybe Var, PreTerm, Bool)
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
    doMakeLazyApp :: Term -> ExprIO Term
    doMakeLazyApp e' = do
      rm <- lift Env.getRefMap
      case preTermLazyCod rm (termTy e') of
        Nothing -> do
          ss <- lift $ preTermToString defaultExprIndent (termTy e')
          lift $ err (exprLoc e) (Recoverable $
            "expected expression to have lazy type, "
            ++ "but type is\n" ++ ss)
        Just (ty', io) -> do
          return (mkTerm (TermLazyApp io (termPre e')) ty' (termIo e' || io))
doTcExpr _ subst _ _ (ExprLazyArrow _ io e) = do
  e' <- tcExpr subst TermTy e
  return (mkTerm (TermLazyArrow io (termPre e')) TermTy (termIo e'))
doTcExpr isTrial subst _ ty (ExprApp f args) = do
  case args of
    [] -> doTcExprApp isTrial subst Nothing ty f args
    [a] -> doTcExprApp isTrial subst (Just (Left a)) ty f args
    a1 : a2 : _ -> doTcExprApp isTrial subst (Just (Right (a1, a2))) ty f args
doTcExpr isTrial subst operandArg _ (ExprImplicitApp f args) = do
  f' <- doTcExprSubst isTrial subst operandArg f
  opn <- if isTrial then return Nothing else lift (typeOperandName (termTy f'))
  if isTrial && isJust opn
  then return (mkTerm TermEmpty (termTy f') False)
  else do
    (r, v, ias) <- case termPre f' of
                    TermImplicitApp False r@(TermRef v _) a -> return (r, v, a)
                    TermImplicitApp False r@(TermData v) a -> return (r, v, a)
                    TermImplicitApp False r@(TermCtor v _) a -> return (r, v, a)
                    _ -> do
                      ss <- lift $ preTermToString defaultExprIndent (termPre f')
                      lift (err (exprLoc f) (
                              Recoverable $
                                "cannot apply term\n"
                                ++ ss ++ "\nto implicit argument(s)"))
    tys <- lift (Env.forceLookupImplicit (varId v))
    let ias' = Map.fromList ias
    let su = foldl (\m (i, _) ->
                      IntMap.insert (varId i) (fromJust (Map.lookup (varName i) ias')) m
                   ) IntMap.empty tys
    let z = zipWith (\x y ->
                      (fst y, (snd y, substPreTerm su (snd x)))
                    ) tys ias
    let m = Map.fromList z
    rm <- lift Env.getRefMap
    let dias = map (\(_, (n, t)) -> (justImplicitVar n, t)) z
    let nameOrd = dependencyOrderArgs (preTermVars rm) dias (map fst z)
    let argsOrd = orderImplicitArgsBy nameOrd args
    (io, tias, tty) <- unifyImplicits (Map.keysSet m) m argsOrd ias (termTy f')
    return (f' { termPre = TermImplicitApp True r tias
               , termTy = tty
               , termIo = termIo f' || io})
  where
    justImplicitVar :: PreTerm -> Maybe Var
    justImplicitVar (TermVar _ v) = Just v
    justImplicitVar _ = error "expected implicit to be a variable"

    orderImplicitArgsBy ::
      [(Int, (Maybe Var, PreTerm), VarName)] ->
      [((Loc, String), Expr)] -> [((Loc, String), Expr)]
    orderImplicitArgsBy [] as = as
    orderImplicitArgsBy ((_, _, n) : ns) as =
      case getRemoveImplicit n as of
        Nothing -> orderImplicitArgsBy ns as
        Just (a, as') -> a : orderImplicitArgsBy ns as'

    getRemoveImplicit ::
      VarName -> [((Loc, String), Expr)] ->
      Maybe (((Loc, String), Expr), [((Loc, String), Expr)])
    getRemoveImplicit _ [] = Nothing
    getRemoveImplicit n (((lo, m), e) : as)
      | n == m = Just (((lo, m), e), as)
      | True = do
          (a, as0) <- getRemoveImplicit n as
          Just (a, ((lo, m), e) : as0)

    unifyImplicits ::
      Set VarName -> Map VarName (PreTerm, PreTerm) ->
      [((Loc, String), Expr)] -> [(VarName, PreTerm)] -> PreTerm ->
      ExprIO (Bool, [(VarName, PreTerm)], PreTerm)
    unifyImplicits _ _ [] imap fty = return (False, imap, fty)
    unifyImplicits remain ias (((lo, na), e) : es) imap fty = do
      case Map.lookup na ias of
        Nothing -> lift (err lo (Fatal $ "unexpected implicit argument name "
                                         ++ quote na))
        Just (a, ty) ->
          if Set.member na remain
            then do
              e0 <- tcExpr subst ty e
              e' <- runUnify a e0
              isu <- getExprSubst
              let ias' = Map.map (\(n, t) -> (n, substPreTerm isu t)) ias
              let imap' = map (\(n, t) -> (n, substPreTerm isu t)) imap
              let fty' = substPreTerm isu fty
              (io, imap'', fty'') <- unifyImplicits (Set.delete na remain) ias' es imap' fty'
              return (io || termIo e', imap'', fty'')
            else
              lift (err lo (Fatal $ "implicit argument " ++ quote na
                                    ++ " is given multiple times"))
      where
        msgPrefix :: ExprIO String
        msgPrefix =
          return $
            "failed unifying values for implicit argument " ++ quote na
        runUnify :: PreTerm -> Term -> ExprIO Term
        runUnify a e0 =
          runExprUnifResult lo False msgPrefix a (termPre e0) >> return e0
doTcExpr _ subst _ ty (ExprSeq (Left e1) e2) =
  let p1 = ParsePatternUnit (exprLoc e1)
  in doTcExpr False subst Nothing ty (ExprSeq (Right (p1, e1)) e2)
doTcExpr _ subst _ ty (ExprSeq (Right (p1, e1)) e2) =
  doTcExpr False subst Nothing ty (ExprCase (patternLoc p1) e1 [Right (p1, e2)])
doTcExpr _ subst _ ty (ExprCase lo expr cases) = do
  expr' <- doTcExprSubst False subst Nothing expr
  newpids <- lift Env.getNextVarId
  --let !() = trace ("expr type is: " ++ preTermToString 0 (termTy expr')) ()
  (cases', ty0) <- checkCases newpids (termTy expr') cases ty
  case ty0 of
    Nothing -> lift (err lo (Recoverable "unable to infer type of expression"))
    Just ty' -> do
      --isu <- getExprSubst
      --let cases'' = substPatternTriples isu cases'
      (ct, io) <- lift (casesToCaseTree cases')
      let c = TermCase (termPre expr') ct
      return (mkTerm c ty' (termIo expr' || io))
  where
    checkCases ::
      VarId -> PreTerm -> [CaseCase] -> Maybe PreTerm ->
      ExprIO ([(Loc, [CasePatternTriple], Maybe Term)], Maybe PreTerm)
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

    doCheckCase ::
      VarId -> PreTerm -> ParsePattern -> Maybe Expr -> Maybe PreTerm ->
      ExprIO (Loc, [CasePatternTriple], Maybe Term)
    doCheckCase newpids t p (Just e) expectedTy = do
      (e', tr) <- dataScope $ do
        (su, p') <- lift (tcPattern newpids t p)
        isu0 <- getExprSubst
        let isu1 = IntMap.restrictKeys su (IntMap.keysSet isu0)
        isu <- lift (runExceptT (mergeExprUnifMaps isu0 isu1))
        case isu of
          Right x -> putExprSubst x
          Left msg -> lift (err (exprLoc e)
                           (Recoverable $ "unification error, " ++ msg))
        let su' = IntMap.union su subst
        b <- case expectedTy of
              Nothing -> doTcExprSubst False su' Nothing e
              Just ty' -> tcExpr su' (substPreTerm su ty') e
        checkNoVariableEscape (patternLoc p) p' (termTy b)
        return (b, patternToTriple su' p')
      return (patternLoc p, tr, Just e')
    doCheckCase newpids t p Nothing _ = do
      lift . Env.scope $ do
        (su, p') <- tcPattern newpids t p
        return (patternLoc p, patternToTriple su p', Nothing)

    patternToTriple :: SubstMap -> Pattern -> [CasePatternTriple]
    patternToTriple su p =
      let p' = p {patternTy = substPreTerm su (patternTy p)}
      in [((Nothing, False), p')]

    checkNoVariableEscape ::
      Loc -> Pattern -> PreTerm -> ExprIO ()
    checkNoVariableEscape plo p t = do
      rm <- lift Env.getRefMap
      let vp = IntSet.difference
                (prePatternVars (patternPre p))
                (preTermVars rm (patternTy p))
      let vt = preTermVars rm t
      let s = IntSet.intersection vp vt
      when (not (IntSet.null s))
        (lift $ err plo (Fatal "pattern variable(s) are escaping scope"))

doTcExprApp ::
  Bool -> SubstMap -> Maybe (Either Expr (Expr, Expr)) ->
  Maybe PreTerm -> Expr -> [Expr] -> ExprIO Term
doTcExprApp isTrial subst operandArg ty f args = do
  f' <- doTcExprSubst False subst operandArg f
  if not isTrial
  then doMakeTermApp f'
  else do
    rm <- lift Env.getRefMap
    case preTermDomCod rm (termTy f') of
      Nothing -> doMakeTermApp f'
      Just (dom, cod, _) ->
        if length dom /= length args
        then
          doMakeTermApp f'
        else do
          asp <- getArgSplit f'
          if isNothing asp
          then makeTrivialApp f' cod
          else
            case preTermDomCod rm cod of
              Nothing -> doMakeTermApp f'
              Just (_, cod', _) -> makeTrivialApp f' cod'
  where
    makeTrivialApp :: Term -> PreTerm -> ExprIO Term
    makeTrivialApp f' cod = do
      opn <- lift (typeOperandName cod)
      if isNothing opn
      then doMakeTermApp f'
      else return (mkTerm TermEmpty cod False)

    getArgSplit :: Term -> ExprIO (Maybe (Expr, [Expr]))
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

    doMakeTermApp :: Term -> ExprIO Term
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
    isPostfixExpr (ExprVar (_, vna)) = isPostfixOp vna && not ('#' `elem` vna)
    isPostfixExpr (ExprImplicitApp v@(ExprVar _) _) = isPostfixExpr v
    isPostfixExpr _ = False

makeTermApp ::
  SubstMap -> Loc ->
  Maybe PreTerm -> Term -> [Expr] -> ExprIO Term
makeTermApp subst flo preTy f0 preArgs = do
  f' <- forceIfLazyTerm f0
  r <- lift Env.getRefMap
  let dc = preTermDomCod r (termTy f')
  case dc of
    Nothing -> do
      ss <- lift $ preTermToString defaultExprIndent (termTy f')
      lift $ err flo
        (Recoverable $
          "expected expression to have function type, "
          ++ "but type is\n" ++ ss)
    Just (d0, c0, io) -> do
      let numPreArgs = length preArgs
      let numMissing = length d0 - numPreArgs
      (d', c') <- if numMissing > 0
                  then lift (splitDom numPreArgs d0 c0 io)
                  else return (d0, c0)
      let (args, postArgs) = splitAt (length d') preArgs
      let ty = if null postArgs then preTy else Nothing
      iv <- lift Env.getImplicitVarMap
      let args0 = dependencyOrderArgsWithOrder
                    appOrdering
                    (preTermVars r . fst)
                    (map (\((x, y), e) ->
                      (x, (y, getAppOrderingWeight r iv y e))) (zip d' args))
                    args
      let args1 = map (\(i, (v, (p, w)), e) -> (i, v, p, w, e)) args0
      let domVars = getDomVars d'
      cod <- forceIfLazyPreTerm c'
      (su, ps, io') <- applyArgs ty domVars False cod IntMap.empty args1
      let ps' = map snd (sortOn fst ps)
      let c = substPreTerm su c'
      if numMissing > 0
      then do
        g <- lift $ preTermPartialApplication
                      io numMissing (termPre f') ps' (termIo f')
        return (mkTerm g c io')
      else do
        let e = TermApp io (termPre f') ps'
        let fap = mkTerm e c (termIo f' || io || io')
        if null postArgs
        then return fap
        else makeTermApp subst flo preTy fap postArgs
  where
    unifyType ::
      Maybe PreTerm -> IntSet -> (Int, Int, Int, Int) ->
      PreTerm -> SubstMap -> ExprIO Bool
    unifyType ty domVars w cod su = do
      case ty of
        Just ty' -> do
          let cod' = substPreTerm su cod
          cw <- uniWeight cod'
          if w < cw
          then return False
          else do
            rm <- lift Env.getRefMap
            let fs = preTermVars rm cod'
            if IntSet.null (IntSet.intersection fs domVars)
            then (tcExprSubstUnify flo ty' cod' >> return True)
            else return False
        Nothing -> return True
      where
        uniWeight :: PreTerm -> ExprIO (Int, Int, Int, Int)
        uniWeight t = do
          rm <- lift Env.getRefMap
          iv <- lift Env.getImplicitVarMap
          let (x, y, z) = getAppAlphaArgumentWeight rm iv t
          return (0, x, y, z)

    getDomVars :: [(Maybe Var, PreTerm)] -> IntSet
    getDomVars [] = IntSet.empty
    getDomVars ((Nothing, _) : vs) = getDomVars vs
    getDomVars ((Just v, _) : vs) = IntSet.insert (varId v) (getDomVars vs)

    applyArgs ::
      Maybe PreTerm -> IntSet -> Bool -> PreTerm -> SubstMap ->
      [(Int, Maybe Var, PreTerm, (Int, Int, Int, Int), Expr)] ->
      ExprIO (SubstMap, [(Int, PreTerm)], Bool)
    applyArgs _ _ _ _ su [] = return (su, [], False)
    applyArgs ty domVars unified cod su ((idx, Nothing, p, w, e) : xs) = do
      unified' <- unifyCod ty domVars unified w cod su
      (e', eio) <- applyTc p w e
      (su', es', io) <- applyArgs ty domVars unified' cod su xs
      return (su', (idx, e') : es', io || eio)
    applyArgs ty domVars unified cod su ((idx, Just v, p, w, e) : xs) = do
      unified' <- unifyCod ty domVars unified w cod su
      (e', eio) <- applyTc p w e
      r <- lift Env.getRefMap
      let xs' = substArgs r (IntMap.singleton (varId v) e') xs
      let su' = IntMap.insert (varId v) e' su
      (su'', es', io) <- applyArgs ty domVars unified' cod su' xs'
      return (su'', (idx, e') : es', io || eio)

    unifyCod ::
      Maybe PreTerm -> IntSet -> Bool -> (Int, Int, Int, Int) ->
      PreTerm -> SubstMap -> ExprIO Bool
    unifyCod ty domVars unified weight cod su = do
      if unified
      then return True
      else unifyType ty domVars weight cod su
            `catchRecoverable` (\_ -> return False)

    applyTc p _ e = do
      isu <- getExprSubst
      let p' = substPreTerm isu p
      fmap (\d -> (termPre d, termIo d)) (tcExpr subst p' e)

    substArgs ::
      RefMap -> SubstMap ->
      [(Int, Maybe Var, PreTerm, (Int, Int, Int, Int), Expr)] ->
      [(Int, Maybe Var, PreTerm, (Int, Int, Int, Int), Expr)]
    substArgs _ _ [] = []
    substArgs r m ((idx, v, p, w, e) : xs) =
      (idx, v, substPreTerm m p, w, e) : substArgs r m xs

    splitDom ::
      Int -> [(Maybe Var, PreTerm)] -> PreTerm -> Bool ->
      TypeCheckIO ([(Maybe Var, PreTerm)], PreTerm)
    splitDom n d c io = do
      let (d0, d1) = splitAt n d
      let !() = assert (length d1 > 0) ()
      rm <- Env.getRefMap
      let vs = foldl (\a (_, t) ->
                        IntSet.union a (preTermVars rm t)
                     ) IntSet.empty d0
      mapM_ (checkNotIn vs) d1
      return (d0, TermArrow io d1 c)
      where
        checkNotIn :: VarIdSet -> (Maybe Var, PreTerm) -> TypeCheckIO ()
        checkNotIn _ (Nothing, _) = return ()
        checkNotIn vs (Just v, _) =
          when (IntSet.member (varId v) vs) $
            err flo $ Fatal $
              "invalid partial application, applied argument \
              \depends on an unapplied argument"

getAppOrderingWeight ::
  RefMap -> Env.ImplicitVarMap -> PreTerm -> Expr -> (Int, Int, Int, Int)
getAppOrderingWeight rm iv ty e =
  let ty' = preTermNormalize rm ty
      (x, y, z) = getAppAlphaArgumentWeight rm iv ty'
  in (funWeight ty' e, x, y, z)
  where
    allImplicits :: PreTerm -> IntSet
    allImplicits p =
      IntSet.filter (flip IntMap.member iv) (preTermVars rm p)

    funWeight :: PreTerm -> Expr -> Int
    funWeight t@(TermArrow _ _ _) x = IntSet.size (funImplicits t x)
    funWeight t@(TermLazyArrow _ _) x = IntSet.size (funImplicits t x)
    funWeight _ x@(ExprFun _ _ _) = funWeightNoType x
    funWeight t x = IntSet.size (funImplicits t x)

    funImplicits :: PreTerm -> Expr -> IntSet
    funImplicits (TermArrow _ d _) (ExprVar (_, n))
      | isOperator n && not ('#' `elem` n) =
          foldl (\s (_, x) ->
                  IntSet.union s (allImplicits x)
                ) IntSet.empty d
    funImplicits (TermArrow _ d _)
        (ExprApp (ExprVar (_, n)) (ExprVar (_, "_") : _))
      | (isPostfixOp n || isLeftAssocFixOp n)
            && not ('#' `elem` n) =
          foldl (\s (_, x) ->
                  IntSet.union s (allImplicits x)
                ) IntSet.empty d
    funImplicits (TermArrow _ d _)
        (ExprApp (ExprVar (_, n)) (_ : ExprVar (_, "_") : _))
      | isRightAssocFixOp n && not ('#' `elem` n) =
          foldl (\s (_, x) ->
                  IntSet.union s (allImplicits x)
                ) IntSet.empty d
    funImplicits (TermArrow _ d c) (ExprFun _ ps b) =
      let s0 = foldl (\s ((_, x), (_, t)) ->
                        if isNothing t
                        then IntSet.union s (allImplicits x)
                        else s
                     ) IntSet.empty (zip d ps)
          s1 = funImplicits c b
      in IntSet.union s0 s1
    funImplicits (TermLazyArrow _ c) b = funImplicits c b
    funImplicits _ _ = IntSet.empty

    funWeightNoType :: Expr -> Int
    funWeightNoType (ExprFun _ ps x) =
      funWeightNoType x + foldl (\a (_, t) ->
                            if isNothing t then a + 1 else a) 0 ps
    funWeightNoType _ = 0

appOrdering ::
  (PreTerm, (Int, Int, Int, Int)) -> (PreTerm, (Int, Int, Int, Int)) -> Ordering
appOrdering (_, n1) (_, n2) = compare n1 n2

tcPattern ::
  VarId -> PreTerm -> ParsePattern -> TypeCheckIO (SubstMap, Pattern)
tcPattern newpids ty p = do
  (su, p')  <- doTcPatternForceLazy False False newpids ty p
  rm <- Env.getRefMap
  --let !() = trace ("type: " ++ preTermToString 0 (patternTy p')) ()
  when (patternPreCanApply (patternPre p') && isArrowType rm (patternTy p')) $ do
    pp <- prePatternToString defaultExprIndent (patternPre p')
    (err (patternLoc p) (Fatal $
      "pattern\n" ++ pp ++ "\nis missing argument(s)"))
  return (su, p')
  where
    isArrowType :: RefMap -> PreTerm -> Bool
    isArrowType rm t = isArrowType' (preTermNormalize rm t)
    isArrowType' :: PreTerm -> Bool
    isArrowType' (TermArrow _ _ _) = True
    isArrowType' (TermLazyArrow _ _) = True
    isArrowType' _ = False

makePatternApp ::
  Loc -> VarId -> PreTerm -> (SubstMap, Pattern) -> [ParsePattern] ->
  TypeCheckIO (SubstMap, Pattern)
makePatternApp flo newpids ty (fsu, f') prePargs = do
  r <- Env.getRefMap
  when (not $ patternPreCanApply (patternPre f')) $ do
    ss <- preTermToString defaultExprIndent (patternTy f')
    err flo (Recoverable $
      "unable to match pattern with inferred type\n" ++ ss)
  let dc = preTermDomCod r (patternTy f')
  case dc of
    Nothing -> do
      ss <- preTermToString defaultExprIndent (patternTy f')
      err flo (Recoverable $
        "unable to match pattern with inferred type\n" ++ ss)
    Just (dom0, cod, io) -> do
      let !() = assert (not io) ()
      when (length dom0 > length prePargs)
        (err flo
          (Fatal $
            "expected " ++ show (length dom0) ++ " argument(s), "
            ++ "but given " ++ show (length prePargs)))
      let (pargs, postPargs) = splitAt (length dom0) prePargs
      let (fcod, _) = preTermFinalCod r cod
      dsu <- tcPatternUnify newpids flo fcod ty
      let dsubts = \(d, t) -> (d, substPreTerm dsu t)
      let args0 = dependencyOrderArgs (preTermVars r) dom0 pargs
      let args = map (\(i, d, a) -> (i, dsubts d, a)) args0
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
      if null postPargs
      then return (psu, p)
      else makePatternApp flo newpids ty (psu, p) postPargs


forceIfLazyPattern :: Pattern -> TypeCheckIO Pattern
forceIfLazyPattern p = do
  rm <- Env.getRefMap
  let n = preTermNormalize rm (patternTy p)
  case n of
    TermLazyArrow _ _ -> doForceIfLazyPattern (patternPre p) n
    _ -> return p

doForceIfLazyPattern ::
  PrePattern -> PreTerm -> TypeCheckIO Pattern
doForceIfLazyPattern p (TermLazyArrow io cod) =
  assert (not io) $ doForceIfLazyPattern (PatternLazyApp p) cod
doForceIfLazyPattern p ty =
  return $ Pattern {patternPre = p, patternTy = ty}


doTcPatternForceLazy ::
  Bool -> Bool -> VarId -> PreTerm -> ParsePattern -> TypeCheckIO (SubstMap, Pattern)
doTcPatternForceLazy hasImplicitApp hasApp newpids ty p = do
  (su, p0) <- doTcPattern hasImplicitApp hasApp newpids ty p
  if patternPreCanApply (patternPre p0)
  then do
    p' <- forceIfLazyPattern p0
    return (su, p')
  else
    return (su, p0)


doTcPattern ::
  Bool -> Bool -> VarId -> PreTerm -> ParsePattern -> TypeCheckIO (SubstMap, Pattern)
doTcPattern _ _ newpids ty (ParsePatternApp f pargs) = do
  f' <- doTcPatternForceLazy False True newpids ty f
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
      isPostfixOp vna && not ('#' `elem` vna)
    isPostfixPattern (ParsePatternImplicitApp v@(ParsePatternVar _) _) =
      isPostfixPattern v
    isPostfixPattern _ = False

doTcPattern _ hasApp newpids ty (ParsePatternImplicitApp f pargs) = do
  (fsu, f') <- doTcPatternForceLazy True hasApp newpids ty f
  rm <- Env.getRefMap
  (r, v, ias) <- case patternPre f' of
                  PatternImplicitApp False r@(PatternCtor v _) a -> return (r, v, a)
                  _ -> do
                    pp <- prePatternToString defaultExprIndent (patternPre f')
                    err (patternLoc f) (Fatal $
                          "cannot apply pattern\n"
                          ++ pp ++ "\nto implicit argument(s)")
  tys0 <- Env.forceLookupImplicit (varId v)
  let (fcod, _) = preTermFinalCod rm (patternTy f')
  dsu <- tcPatternUnify newpids (patternLoc f) fcod ty
  let tys = map (\(d, t) -> (d, substPreTerm dsu t)) tys0
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
  let trp = foldr (\((x, _), s) (a1, a2, ss) ->
                    if Set.member x pnames
                    then
                      (x : a1, fromJust (Map.lookup x pmap) : a2, s : ss)
                    else
                      (a1, a2, ss)
                  ) ([], [], []) (zip ias tys0)
  let (pns, pargs', tys') = trp
  let args0 = dependencyOrderArgs (preTermVars rm) (map (\(n, d) -> (Just n, d)) tys') pargs'
  let args = map (\(d, (i, _, a)) -> (i, d, a)) (zip z args0)
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
    getImplicitNames ::
      Set VarName -> Set VarName ->
      [((Loc, String), ParsePattern)] -> TypeCheckIO (Set VarName)
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
doTcPattern hasImplicitApp hasApp newpids ty (ParsePatternVar (lo, na0)) = do
  (x, na) <- if not (isOperator na0) || '#' `elem` na0
             then do
              let (na1, opty) = operandSplit na0
              na1' <- expandVarNameEnv na1
              na2 <- updateOperandTypeString (lo, na1') opty
              fmap (\x -> (x, na2)) (lookupEnv na2)
             else do
              na <- getDataCtorMatching
              x1 <- lookupEnv na
              return (x1, na)
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
      ss <- preTermToString defaultExprIndent ty
      if hasImplicitApp || hasApp
      then err lo (Recoverable $ "expected " ++ quote na0
                               ++ " to be constructor of\n" ++ ss)
      else do
        v <- insertNonblankFreshVariable (lo, na) ty
        return (IntMap.empty, Pattern {patternPre = PatternVar v,
                                       patternTy = ty})
  where
    getDataCtorMatching :: TypeCheckIO VarName
    getDataCtorMatching = do
      cs <- getDataCtors
      na0' <- expandVarNameEnv na0
      case cs of
        Nothing -> return na0'
        Just cs' -> do
          case filter (isPrefixOf na0') (map varName cs') of
            [m] -> do
              let (_, m1) = span (/='#') m
              return (na0' ++ m1)
            _ -> return na0'

    getDataCtors :: TypeCheckIO (Maybe [Var])
    getDataCtors = do
      rm <- Env.getRefMap
      case preTermCodRootType rm ty of
        Just (TermData d, _) -> do
          tcUnfinishedData (varId d)
          c <- Env.forceLookupDataCtor (varId d)
          return (Just c)
        _ -> return Nothing

    ctorFromVarStatus :: VarStatus -> TypeCheckIO (Var, VarId, PreTerm)
    ctorFromVarStatus x = do
      x' <- varStatusTerm x
      case x' of
        Term {termPre = TermCtor v did, termTy = ty'} ->
          return (v, did, ty')
        _ -> error $ "expected var status to return ctor " ++ show x

    checkCtorType ::
      Var -> VarId -> PreTerm -> TypeCheckIO (SubstMap, Pattern)
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

    makeImplicitApp ::
      Var -> VarId -> PreTerm -> TypeCheckIO Pattern
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

    getImplicit :: (Var, PreTerm) -> TypeCheckIO (Var, PrePattern)
    getImplicit (v, _) = do
      i <- Env.freshVarId
      l <- Env.getNextLocalVarName
      let v' = mkVar i (varName v ++ "_" ++ l)
      return (v, PatternVar v')
doTcPattern _ _ newpids ty (ParsePatternEmpty lo) = do
  verifyIsEmptyType lo newpids ty
  return (IntMap.empty, Pattern {patternPre = PatternEmpty,
                                 patternTy = ty})
doTcPattern _ _ newpids ty (ParsePatternUnit lo) = do
  su <- tcPatternUnify newpids lo ty TermUnitTy
  return (su, Pattern {patternPre = PatternUnit, patternTy = TermUnitTy})

tcPatternArgs ::
  VarId -> SubstMap -> SubstMap -> [(Int, (Maybe Var, PreTerm), ParsePattern)] ->
  TypeCheckIO (SubstMap, SubstMap, [(Int, (Maybe Var, Pattern))])
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

isEmptyType :: VarId -> PreTerm -> TypeCheckIO Bool
isEmptyType newpids ty = do
  r <- Env.getRefMap
  let ty' = preTermNormalize r ty
  tcs <- getTypeCtors ty'
  case tcs of
    Just is -> foldlM (meetCtorId ty') True is
    Nothing -> return False
  where
    getTypeCtors :: PreTerm -> TypeCheckIO (Maybe [Var])
    getTypeCtors (TermApp _ f _) = getTypeCtors f
    getTypeCtors (TermLazyApp _ f) = getTypeCtors f
    getTypeCtors (TermImplicitApp _ f _) = getTypeCtors f
    getTypeCtors (TermData v) =
      fmap Just (Env.forceLookupDataCtor (varId v))
    getTypeCtors _ = return Nothing

    meetCtorId :: PreTerm -> Bool -> Var -> TypeCheckIO Bool
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

verifyIsEmptyType :: Loc -> VarId -> PreTerm -> TypeCheckIO ()
verifyIsEmptyType lo newpids ty = do
  e <- isEmptyType newpids ty
  when (not e) $ do
    ss <- preTermToString defaultExprIndent ty
    err lo (Recoverable $
              "unable to match empty pattern with inferred type\n" ++ ss)

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

dependencyOrderArgsWithOrder ::
  (t -> t -> Ordering) -> (t -> VarIdSet) -> [(Maybe Var, t)] -> [a] ->
  [(Int, (Maybe Var, t), a)]
dependencyOrderArgsWithOrder = \order f ps es -> do
  let vs = paramVars ps
  let ps' = zipParamVars f 0 ps es
  doRearrange order vs ps'
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

    doRearrange :: (t -> t -> Ordering) -> VarIdSet ->
                   [(Int, (Maybe Var, t, VarIdSet), a)] ->
                   [(Int, (Maybe Var, t), a)]
    doRearrange _ _ [] = []
    doRearrange order vs ps =
      let fs0 = findFreeIndices 0 vs ps
          fs = sortBy (\x y -> order (snd x) (snd y)) fs0
          (vs', n, ps') = nextFreeParamAt (fst (head fs)) vs ps
      in n : doRearrange order vs' ps'

    nextFreeParamAt ::
      Int -> VarIdSet -> [(Int, (Maybe Var, t, VarIdSet), a)] ->
      (VarIdSet, (Int, (Maybe Var, t), a),
        [(Int, (Maybe Var, t, VarIdSet), a)])
    nextFreeParamAt _ _ [] = error "is there a cyclic parm dependency?"
    nextFreeParamAt 0 vs ((idx, (v, p, _), e) : ps) =
      case v of
        Nothing -> (vs, (idx, (v, p), e), ps)
        Just v' -> (IntSet.delete (varId v') vs, (idx, (v, p), e), ps)
    nextFreeParamAt i vs (x : ps) =
      let (vs', n, ps') = nextFreeParamAt (i-1) vs ps
      in (vs', n, x : ps')

    findFreeIndices ::
      Int -> VarIdSet -> [(Int, (Maybe Var, t, VarIdSet), a)] -> [(Int, t)]
    findFreeIndices _ _ [] = []
    findFreeIndices idx vs ((_, (_, p, is), _) : ps) =
      if IntSet.disjoint vs is
        then (idx, p) : findFreeIndices (idx + 1) vs ps
        else findFreeIndices (idx + 1) vs ps
