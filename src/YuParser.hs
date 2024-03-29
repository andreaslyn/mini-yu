module YuParser (runParse)
where

import Str (stringToYuChars)
import Text.Parsec
import YuScanner
import Loc
import Control.Monad
import Control.Monad.IO.Class
import ParseTree

type YuParsec a = Parsec [Tok] () a

tokToPos :: YuParsec (Tok -> SourcePos)
tokToPos = do
  p <- getPosition
  return (locToSourcePos (sourceName p) . tokLoc)

yuKeyTok :: TokType -> YuParsec Loc
yuKeyTok ty = do
  f <- tokToPos
  token (show . tokType) f cmpTy
  where
    cmpTy t = if tokType t == ty then Just (tokLoc t) else Nothing

yuPostfixOpTok :: YuParsec (Loc, String)
yuPostfixOpTok = do
  f <- tokToPos
  token (show . tokType) f getVar
  where
    getVar t = case tokType t of
                TokPostfixOp s -> Just (tokLoc t, s)
                _ -> Nothing

yuVarTok :: YuParsec (Loc, String)
yuVarTok = do
  f <- tokToPos
  token (show . tokType) f getVar
  where
    getVar t = case tokType t of
                TokVar s -> Just (tokLoc t, s)
                _ -> Nothing

yuInfixOpTok :: Int -> YuParsec (Loc, String)
yuInfixOpTok idx = do
  f <- tokToPos
  token (\x -> show (tokType x)) f getVar
  where
    getVar t =
      case idx of
        6 -> case tokType t of
                TokOp6 s -> Just (tokLoc t, s)
                _ -> Nothing
        5 -> case tokType t of
                TokOp5 s -> Just (tokLoc t, s)
                _ -> Nothing
        4 -> case tokType t of
                TokOp4 s -> Just (tokLoc t, s)
                _ -> Nothing
        3 -> case tokType t of
                TokOp3 s -> Just (tokLoc t, s)
                _ -> Nothing
        2 -> case tokType t of
                TokOp2 s -> Just (tokLoc t, s)
                _ -> Nothing
        1 -> case tokType t of
                TokOp1 s -> Just (tokLoc t, s)
                _ -> Nothing
        _ -> error ("invalid operator index " ++ show idx)

yuOpTok :: YuParsec (Loc, String)
yuOpTok = do
  f <- tokToPos
  token (\x -> show (tokType x)) f getVar
  where
    getVar t =
      case tokType t of
        TokOp6 s -> Just (tokLoc t, s)
        TokOp5 s -> Just (tokLoc t, s)
        TokOp4 s -> Just (tokLoc t, s)
        TokOp3 s -> Just (tokLoc t, s)
        TokOp2 s -> Just (tokLoc t, s)
        TokOp1 s -> Just (tokLoc t, s)
        _ -> Nothing

yuStringLitTok :: YuParsec (Loc, String)
yuStringLitTok = do
  f <- tokToPos
  token (show . tokType) f getStr
  where
    getStr t = case tokType t of
                TokStringLit s -> Just (tokLoc t, s)
                _ -> Nothing

parseTypeSpec :: YuParsec Expr
parseTypeSpec = do
  _ <- yuKeyTok TokColon
  parseExpr

parseProgram :: YuParsec Program
parseProgram = do
  is <- many parseModuleIntro
  ds <- many parseDef
  _ <- yuKeyTok TokEof
  return (is, ds)

parseModuleIntro :: YuParsec ModuleIntro
parseModuleIntro = do
  _ <- yuKeyTok TokImport
  m <- optionMaybe (try $ yuVarTok <* yuKeyTok TokEqGreater)
  path <- yuVarTok
  im <- many importElem
  ex0 <- optionMaybe (yuKeyTok TokExport >> many1 exportElem)
  let ex = case ex0 of
            Nothing -> []
            Just es -> es
  return (m, path, im ++ ex)
  where
    importElem :: YuParsec (Loc, String, Bool)
    importElem = do
      _ <- yuKeyTok TokOf
      (lo, na) <- parseVarOrOp
      return (lo, na, False)

    exportElem :: YuParsec (Loc, String, Bool)
    exportElem = do
      _ <- yuKeyTok TokOf
      (lo, na) <- parseVarOrOp
      return (lo, na, True)


parseDef :: YuParsec Def
parseDef = parseExternDef <|> parseValDef <|> parseDataDef

parseDataDef :: YuParsec Def
parseDataDef = do
  (isPure, d) <- parseDataDecl
  v <- parseCtorDecls
  return (DefData isPure d v)

parseCtorDecls :: YuParsec [Decl]
parseCtorDecls = try emptyCtor <|> many1 doCtor
  where
    emptyCtor :: YuParsec [Decl]
    emptyCtor = do
      _ <- yuKeyTok TokOf
      _ <- yuKeyTok TokCurlyL
      _ <- yuKeyTok TokCurlyR
      return []

    doCtor :: YuParsec Decl
    doCtor = yuKeyTok TokOf >> parseDecl

parseVarOrOp :: YuParsec (Loc, String)
parseVarOrOp = do
  pre <- optionMaybe (yuKeyTok TokParenL)
  case pre of
    Nothing -> yuVarTok
    Just _ -> do
      v <- yuOpTok <|> yuPostfixOpTok
      _ <- yuKeyTok TokParenR
      return v

parseDecl :: YuParsec Decl
parseDecl = do
  var <- parseVarOrOp
  imps <- many parseImplicit
  ty <- parseTypeSpec
  return (Decl var (concat imps) ty)
  where
    parseImplicit :: YuParsec [VarListElem]
    parseImplicit = do
      _ <- yuKeyTok TokSquareL
      xs <- varListElem
      _ <- yuKeyTok TokSquareR
      return xs

    varListElem :: YuParsec [VarListElem]
    varListElem = do
      vars <- many1 parseVarOrOp
      mty <- optionMaybe parseTypeSpec
      case mty of
        Nothing -> return (zip vars (repeat Nothing))
        Just ty -> return (zip vars (repeat $ Just ty))

parseDataDecl :: YuParsec (Bool, Decl)
parseDataDecl = impureData <|> pureData
  where
    pureData :: YuParsec (Bool, Decl)
    pureData = do
      _ <- yuKeyTok TokData
      d <- parseDecl
      return (True, d)
    impureData :: YuParsec (Bool, Decl)
    impureData = do
      _ <- yuKeyTok TokDataDotDot
      d <- parseDecl
      return (False, d)

parseVarListLoc :: YuParsec (Loc, VarList)
parseVarListLoc = do
  xs <- many1 varListElem
  let lo = fst (head xs)
  let xs' = map snd xs
  return (lo, concat xs')
  where
    varListElem :: YuParsec (Loc, [VarListElem])
    varListElem = try varListElem1 <|> varListElemMany

    varListElemMany :: YuParsec (Loc, [VarListElem])
    varListElemMany = do
      lo <- yuKeyTok TokParenL
      vars <- many1 parseVarOrOp
      ty <- case vars of
              [_] -> optionMaybe parseTypeSpec
              _ -> fmap Just parseTypeSpec
      _ <- yuKeyTok TokParenR
      return (lo, zip vars (repeat ty))

    varListElem1 :: YuParsec (Loc, [VarListElem])
    varListElem1 = do
      var <- parseVarOrOp
      return (fst var, [(var, Nothing)])

parseValDecl :: YuParsec (Bool, Decl)
parseValDecl = impureVal <|> pureVal
  where
    pureVal :: YuParsec (Bool, Decl)
    pureVal = do
      _ <- yuKeyTok TokVal
      d <- parseDecl
      return (True, d)
    impureVal :: YuParsec (Bool, Decl)
    impureVal = do
      _ <- yuKeyTok TokValDotDot
      d <- parseDecl
      return (False, d)

parseValDef :: YuParsec Def
parseValDef = do
  (isPure, d) <- parseValDecl
  lets <- many1 parseValCase
  return (DefVal isPure d lets)

parseExternDef :: YuParsec Def
parseExternDef = do
  _ <- yuKeyTok TokExtern
  d <- parseDecl
  return (DefExtern d)

parseValCase :: YuParsec ValCase
parseValCase = do
  lo <- yuKeyTok TokLet
  (impls, args) <- parseLetPattern
  d <- optionMaybe letDef
  case args of
    [] -> return (ValCase lo impls Nothing d)
    _ -> return (ValCase lo impls (Just args) d)
  where
    parseLetPattern ::
      YuParsec ([((Loc, String), ParsePattern)], [ParsePattern])
    parseLetPattern = do
      imps <- many parseImplicit
      args <- many parsePatternLeaf
      return (imps, args)

    parseImplicit :: YuParsec ((Loc, String), ParsePattern)
    parseImplicit = do
      _ <- yuKeyTok TokSquareL
      imp <- parseImplicitAssign
      _ <- yuKeyTok TokSquareR
      return imp

    parseImplicitAssign :: YuParsec ((Loc, String), ParsePattern)
    parseImplicitAssign = do
      v <- yuVarTok
      _ <- yuKeyTok TokColonEq
      p <- parsePattern
      return (v, p)

    letDef :: YuParsec (Expr, OptWhereClause)
    letDef = do
      _ <- yuKeyTok TokEqGreater
      e <- parseExpr
      w <- optionMaybe parseWhere
      return (e, w)

parseWhere :: YuParsec [Def]
parseWhere = do
  _ <- yuKeyTok TokWhere
  xs <- many parseDef
  _ <- yuKeyTok TokEnd
  return xs

parseExpr :: YuParsec Expr
parseExpr = parseDoExpr

parseDoExpr :: YuParsec Expr
parseDoExpr = doParseDoExpr <|> parseFunExpr

doParseDoExpr :: YuParsec Expr
doParseDoExpr = yuKeyTok TokBackslash >> parseExpr

parseFunExpr :: YuParsec Expr
parseFunExpr = do
  d <- parseFunDomMaybe
  case d of
    Nothing -> parseExprSeq
    Just (lo, xs) -> do
      e <- parseFunExpr
      return (ExprFun Nothing lo xs e)
  where
    parseFunDomMaybe :: YuParsec (Maybe (Loc, VarList))
    parseFunDomMaybe =
      optionMaybe $ try $ do
        d <- parseVarListLoc
        _ <- yuKeyTok TokEqGreater
        return d

parseDoOr :: YuParsec Expr -> YuParsec Expr
parseDoOr e = doParseDoExpr <|> e

parseArrowExpr :: YuParsec Expr
parseArrowExpr = do
  first <- optionMaybe (try paramElemWithTypeSpec)
  case first of
    Just (lo, vars) -> do
      ar <- optionMaybe $ do
              _ <- yuKeyTok TokAmp
              (b, rest, c) <- manyArgsArrow
              return (ExprArrow lo b (vars ++ rest) c)
      case ar of
        Just ar' -> return ar'
        Nothing -> do
          b <- parseArrowSymbol
          c <- parseDoExpr
          return (ExprArrow lo b vars c)
    Nothing -> do
      lzOrDl <- optionMaybe tryLazyOrDelayArg
      case lzOrDl of
        Just (True, lo, b) -> do
          c <- parseDoExpr
          return (ExprLazyArrow lo b c)
        Just (False, lo, b) -> do
          c <- parseDoExpr
          return (ExprDelayArrow lo b c)
        Nothing -> do
          e <- parseOp6Expr
          ar <- optionMaybe $ do
                  _ <- yuKeyTok TokAmp
                  (b, rest, c) <- manyArgsArrow
                  return (ExprArrow (exprLoc e) b (Left e : rest) c)
          case ar of
            Just ar' -> return ar'
            Nothing -> do
              a <- optionMaybe $ do
                    x <- parseArrowSymbol
                    y <- parseDoExpr
                    return (x, y)
              case a of
                Nothing -> return e
                Just (b, a') -> return (ExprArrow (exprLoc e) b [Left e] a')
  where
    tryLazyOrDelayArg :: YuParsec (Bool, Loc, Bool)
    tryLazyOrDelayArg = try lazyArg <|> try delayArg

    lazyArg :: YuParsec (Bool, Loc, Bool)
    lazyArg = do
      lo <- yuKeyTok TokSquareL
      _ <- yuKeyTok TokSquareR
      b <- parseArrowSymbol
      return (True, lo, b)

    delayArg :: YuParsec (Bool, Loc, Bool)
    delayArg = do
      lo <- yuKeyTok TokParenL
      _ <- yuKeyTok TokParenR
      b <- parseArrowSymbol
      return (False, lo, b)

    parseArrowSymbol :: YuParsec Bool
    parseArrowSymbol =
      (yuKeyTok TokDashGreater >> return False)
      <|> (yuKeyTok TokDashGreaterIo >> return True) 

    manyArgsArrow :: YuParsec (Bool, [ExprListTypedElem], Expr)
    manyArgsArrow = do
      es0 <- paramElem `sepBy` yuKeyTok TokAmp
      let es = map snd es0
      b <- parseArrowSymbol
      a <- parseDoExpr
      return (b, concat es, a)

    paramElem :: YuParsec (Loc, [ExprListTypedElem])
    paramElem = try paramElemWithTypeSpec <|> paramElemWithoutTypeSpec

    paramElemWithTypeSpec :: YuParsec (Loc, [ExprListTypedElem])
    paramElemWithTypeSpec = do
      lo <- yuKeyTok TokParenL
      (_, p) <- paramElemVarsTypeSpec
      _ <- yuKeyTok TokParenR
      return (lo, p)

    paramElemVarsTypeSpec :: YuParsec (Loc, [ExprListTypedElem])
    paramElemVarsTypeSpec = do
      v <- many1 parseVarOrOp
      let lo = fst (head v)
      ty <- parseTypeSpec
      return (lo, zipWith (\x t -> Right (x, t)) v (repeat ty))

    paramElemWithoutTypeSpec :: YuParsec (Loc, [ExprListTypedElem])
    paramElemWithoutTypeSpec = liftM (\x -> (exprLoc x, [Left x])) parseOp6Expr

yuChainl1 :: YuParsec Expr -> YuParsec (Expr -> Expr -> Expr) -> YuParsec Expr
yuChainl1 p op = do { x <- p; rest x }
  where
    rest x = dorest x <|> return x
    dorest x = do
      f <- op
      y0 <- optionMaybe doParseDoExpr
      case y0 of
        Nothing -> do { y <- p; rest (f x y) }
        Just y -> return (f x y)

yuChainr1 :: YuParsec Expr -> YuParsec (Expr -> Expr -> Expr) -> YuParsec Expr
yuChainr1 p op = scan
  where
    scan = do { x <- p; rest x }
    rest x = dorest x <|> return x
    dorest x = do
      f <- op
      y0 <- optionMaybe doParseDoExpr
      case y0 of
        Nothing -> do { y <- scan; return (f x y) }
        Just y -> return (f x y)

parseOp6Expr :: YuParsec Expr
parseOp6Expr =
  parseOp5Expr `yuChainl1` op6
  where
    op6 :: YuParsec (Expr -> Expr -> Expr)
    op6 = do
      v <- yuInfixOpTok 6
      return (\e1 e2 -> ExprApp Nothing (ExprVar Nothing v) [e1, e2])

parseOp5Expr :: YuParsec Expr
parseOp5Expr =
  parseOp4Expr `yuChainr1` op5
  where
    op5 :: YuParsec (Expr -> Expr -> Expr)
    op5 = do
      v <- yuInfixOpTok 5
      return (\e1 e2 -> ExprApp Nothing (ExprVar Nothing v) [e1, e2])

parseOp4Expr :: YuParsec Expr
parseOp4Expr =
  parseOp3Expr `yuChainl1` op4
  where
    op4 :: YuParsec (Expr -> Expr -> Expr)
    op4 = do
      v <- yuInfixOpTok 4
      return (\e1 e2 -> ExprApp Nothing (ExprVar Nothing v) [e1, e2])

parseOp3Expr :: YuParsec Expr
parseOp3Expr =
  parseOp2Expr `yuChainr1` op3
  where
    op3 :: YuParsec (Expr -> Expr -> Expr)
    op3 = do
      v <- yuInfixOpTok 3
      return (\e1 e2 -> ExprApp Nothing (ExprVar Nothing v) [e1, e2])

parseOp2Expr :: YuParsec Expr
parseOp2Expr =
  parseOp1Expr `yuChainl1` op2
  where
    op2 :: YuParsec (Expr -> Expr -> Expr)
    op2 = do
      v <- yuInfixOpTok 2
      return (\e1 e2 -> ExprApp Nothing (ExprVar Nothing v) [e1, e2])

parseOp1Expr :: YuParsec Expr
parseOp1Expr =
  parsePrefixOpExpr `yuChainr1` op1
  where
    op1 :: YuParsec (Expr -> Expr -> Expr)
    op1 = do
      v <- yuInfixOpTok 1
      return (\e1 e2 -> ExprApp Nothing (ExprVar Nothing v) [e1, e2])

parsePrefixOpExpr :: YuParsec Expr
parsePrefixOpExpr = do
  v <- optionMaybe yuOpTok
  case v of
    Nothing -> parseAppExpr
    Just v' -> do
      e <- parseDoOr parsePrefixOpExpr
      return (ExprApp Nothing (ExprVar Nothing v') [e])

parseAppExpr :: YuParsec Expr
parseAppExpr = do
  e <- parseExprLeaf
  parseApp e
  where
    parseApp :: Expr -> YuParsec Expr
    parseApp e =
      parsePostfixNormalAndDo e
      <|> parseAppDo e
      <|> parseAppNormal e
      <|> parseAppLazyOrAppImplicit e
      <|> return e

    appArg :: YuParsec Expr
    appArg = parseExprLeaf

    appImplicit :: YuParsec ((Loc, String), Expr)
    appImplicit = do
      _ <- yuKeyTok TokSquareL
      arg <- getNamedArg
      _ <- yuKeyTok TokSquareR
      return arg

    parsePostfixNormalAndDo :: Expr -> YuParsec Expr
    parsePostfixNormalAndDo e = do
      op <- yuPostfixOpTok
      imps <- many appImplicit
      args <- many appArg
      ofArg <- optionMaybe doParseDoExpr
      case imps of
        [] ->
          case ofArg of
            Nothing -> parseApp (ExprApp Nothing (ExprVar Nothing op) (e : args))
            Just x ->  parseApp (ExprApp Nothing (ExprVar Nothing op) (e : args ++ [x]))
        _ ->
          case ofArg of
            Nothing ->
              parseApp
                (ExprApp Nothing
                  (ExprImplicitApp Nothing (ExprVar Nothing op) imps) (e : args))
            Just x ->
              parseApp
                (ExprApp Nothing
                  (ExprImplicitApp Nothing (ExprVar Nothing op) imps) (e : args ++ [x]))

    parseAppNormal :: Expr -> YuParsec Expr
    parseAppNormal e = do
      args <- many1 appArg
      ofArg <- optionMaybe doParseDoExpr
      case ofArg of
        Nothing -> parseApp (ExprApp Nothing e args)
        Just x -> parseApp (ExprApp Nothing e (args ++ [x]))

    parseAppDo :: Expr -> YuParsec Expr
    parseAppDo e = do
      a <- doParseDoExpr
      return (ExprApp Nothing e [a])

    parseAppImplicit :: Expr -> YuParsec Expr
    parseAppImplicit e = do
      args <- many1 appImplicit
      parseApp (ExprImplicitApp Nothing e args)

    parseAppLazyOrAppImplicit :: Expr -> YuParsec Expr
    parseAppLazyOrAppImplicit e = do
      m <- optionMaybe $ try $ do
            _ <- yuKeyTok TokSquareL
            yuKeyTok TokSquareR
      case m of
        Nothing -> parseAppImplicit e
        Just _ -> parseApp (ExprLazyApp Nothing e)

    getNamedArg :: YuParsec ((Loc, String), Expr)
    getNamedArg = do
      v <- parseVarOrOp
      _ <- yuKeyTok TokColonEq
      t <- parseExpr
      return (v, t)

parseExprLeaf :: YuParsec Expr
parseExprLeaf =
      parseExprUnitElem
  <|> parseExprUnitTy
  <|> parseExprVar
  <|> parseParenExpr
  <|> parseCaseExpr
  <|> parseExprTy
  <|> parseExprStr

parseExprUnitElem :: YuParsec Expr
parseExprUnitElem = try $ do
  lo <- yuKeyTok TokParenL
  _ <- yuKeyTok TokParenR
  return (ExprUnitElem lo)

parseExprUnitTy :: YuParsec Expr
parseExprUnitTy = try $ do
  lo <- yuKeyTok TokCurlyL
  _ <- yuKeyTok TokCurlyR
  return (ExprUnitTy lo)

parseExprTy :: YuParsec Expr
parseExprTy = liftM ExprTy (yuKeyTok TokTy)

parseExprStr :: YuParsec Expr
parseExprStr = do
  (lo, s) <- yuStringLitTok
  return (ExprApp Nothing (ExprVar Nothing (lo, "mk.yu/Str/Str"))
            [yuCharsToExpr (stringToYuChars s) lo])
  where
    yuCharsToExpr :: [String] -> Loc -> Expr
    yuCharsToExpr [] lo =
      ExprVar Nothing (lo, "nil.yu/List/List")
    yuCharsToExpr (c : cs) lo =
      ExprApp Nothing
        (ExprVar Nothing (lo, "::.yu/List/List#List.yu/List/List"))
        [ExprVar Nothing (lo, c), yuCharsToExpr cs lo]

parseExprVar :: YuParsec Expr
parseExprVar = liftM (ExprVar Nothing) yuVarTok

exprToPattern :: Expr -> YuParsec ParsePattern
exprToPattern (ExprApp _ e es) = do
  e' <- exprToPattern e
  es' <- mapM exprToPattern es
  return (ParsePatternApp e' es')
exprToPattern (ExprLazyApp _ e) = do
  e' <- exprToPattern e
  return (ParsePatternLazyApp e')
exprToPattern (ExprImplicitApp _ e es) = do
  e' <- exprToPattern e
  es' <- mapM (\(n, x) -> fmap ((,) n) (exprToPattern x)) es
  return (ParsePatternImplicitApp e' es')
exprToPattern (ExprVar _ n) = return (ParsePatternVar n)
exprToPattern (ExprUnitElem lo) = return (ParsePatternUnit lo)
exprToPattern (ExprUnitTy lo) = return (ParsePatternEmpty lo)
exprToPattern _ = fail "invalid pattern"

parseExprSeq :: YuParsec Expr
parseExprSeq = do
  e0 <- parseArrowExpr
  m <- optionMaybe $ do
          _ <- yuKeyTok TokColonEq
          e1 <- parseArrowExpr
          _ <- yuKeyTok TokSemiColon
          e2 <- parseExpr
          p0 <- exprToPattern e0
          return (ExprSeq Nothing (Right (p0, e1)) e2)
  case m of
    Just e -> return e
    Nothing -> do
      c <- optionMaybe (yuKeyTok TokSemiColon)
      case c of
        Nothing -> return e0
        Just _ -> do
          e <- parseExpr
          return (ExprSeq Nothing (Left e0) e)

parseParenExpr :: YuParsec Expr
parseParenExpr = do
  _ <- yuKeyTok TokParenL
  try parenOp <|> parenExpr
  where
    parenOp :: YuParsec Expr
    parenOp = do
      v <- yuOpTok <|> yuPostfixOpTok
      _ <- yuKeyTok TokParenR
      return (ExprVar Nothing v)

    parenExpr :: YuParsec Expr
    parenExpr = do
      e <- parseExpr
      _ <- yuKeyTok TokParenR
      return e

parseCaseExpr :: YuParsec Expr
parseCaseExpr = do
  lo <- yuKeyTok TokCase
  e <- parseExpr
  ofs <- many1 caseCase
  _ <- yuKeyTok TokEnd
  return (ExprCase Nothing lo e ofs)
  where
    caseCase :: YuParsec CaseCase
    caseCase = do
      _ <- yuKeyTok TokOf
      p <- parsePattern
      e <- optionMaybe caseDef
      case e of
        Nothing -> return (Left p)
        Just e' -> return (Right (p, e'))

    caseDef :: YuParsec Expr
    caseDef = do
      _ <- yuKeyTok TokEqGreater
      e <- parseExpr
      return e

parsePattern :: YuParsec ParsePattern
parsePattern = parseOp6Expr >>= exprToPattern

parsePatternLeaf :: YuParsec ParsePattern
parsePatternLeaf = parseExprLeaf >>= exprToPattern

runParse' :: FilePath -> [Tok] -> Either String Program
runParse' p ts =
  case parse parseProgram p ts of
    Left e ->
      let pos = errorPos e
          lin = sourceLine pos
          col = sourceColumn pos
      in Left (p ++ ":" ++ show lin ++ ":" ++ show col ++ ": syntax error")
    Right prog -> Right prog

runParse :: MonadIO m => FilePath -> m (Either String Program)
runParse p = do
  ts <- runScan p
  case ts of
    Left e -> return (Left e)
    Right ts' -> return (runParse' p ts')
