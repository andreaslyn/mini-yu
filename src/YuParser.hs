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

yuVarTok :: YuParsec (Loc, String)
yuVarTok = do
  f <- tokToPos
  token (show . tokType) f getVar
  where
    getVar t = case tokType t of
                TokVar s -> Just (tokLoc t, s)
                _ -> Nothing

yuOpTok :: Int -> YuParsec (Loc, String)
yuOpTok idx = do
  f <- tokToPos
  token (\x -> show (tokType x)) f getVar
  where
    getVar t =
      case idx of
        6 -> case tokType t of
                TokOp6 s -> Just (tokLoc t, '_' : s ++ "_")
                _ -> Nothing
        5 -> case tokType t of
                TokOp5 s -> Just (tokLoc t, '_' : s ++ "_")
                _ -> Nothing
        4 -> case tokType t of
                TokOp4 s -> Just (tokLoc t, '_' : s ++ "_")
                _ -> Nothing
        3 -> case tokType t of
                TokOp3 s -> Just (tokLoc t, '_' : s ++ "_")
                _ -> Nothing
        2 -> case tokType t of
                TokOp2 s -> Just (tokLoc t, '_' : s ++ "_")
                _ -> Nothing
        1 -> case tokType t of
                TokOp1 s -> Just (tokLoc t, '_' : s ++ "_")
                _ -> Nothing
        _ -> error ("invalid operator index " ++ show idx)

yuPreOpTok :: YuParsec (Loc, String)
yuPreOpTok = do
  f <- tokToPos
  token (\x -> show (tokType x)) f getVar
  where
    getVar t =
      case tokType t of
        TokOp6 s -> Just (tokLoc t, s ++ "_")
        TokOp5 s -> Just (tokLoc t, s ++ "_")
        TokOp4 s -> Just (tokLoc t, s ++ "_")
        TokOp3 s -> Just (tokLoc t, s ++ "_")
        TokOp2 s -> Just (tokLoc t, s ++ "_")
        TokOp1 s -> Just (tokLoc t, s ++ "_")
        _ -> Nothing

yuPostfixOpTok :: YuParsec (Loc, String)
yuPostfixOpTok = do
  (lo, op) <- keyTok <|> yuVarTok
  return (lo, '_' : op)
  where
    keyTok :: YuParsec (Loc, String)
    keyTok =
          (yuKeyTok TokTy >>= \lo -> return (lo, "Ty"))
      <|> (yuKeyTok TokUnitTy >>= \lo -> return (lo, "Unit"))
      <|> (yuKeyTok TokUnitElem >>= \lo -> return (lo, "unit"))

yuPreOpTok' :: YuParsec (Loc, String)
yuPreOpTok' = do
  f <- tokToPos
  (lo, s) <- token (\x -> show (tokType x)) f getVar
  return (lo, s)
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
  is <- many parseImport
  ds <- many parseDef
  _ <- yuKeyTok TokEof
  return (is, ds)

parseImport :: YuParsec ImportPath
parseImport = do
  _ <- yuKeyTok TokImport
  yuStringLitTok

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
      _ <- yuKeyTok TokLet
      _ <- yuKeyTok TokParenL
      _ <- yuKeyTok TokParenR
      return []

    doCtor :: YuParsec Decl
    doCtor = yuKeyTok TokLet >> parseDecl

parseVarOrPrefix :: YuParsec (Loc, String)
parseVarOrPrefix = do
  pre <- optionMaybe (yuKeyTok TokParenL)
  case pre of
    Nothing -> yuVarTok
    Just _ -> do
      v <- yuPreOpTok'
      _ <- yuKeyTok TokParenR
      return v

parseDecl :: YuParsec Decl
parseDecl = do
  var <- parseVarOrPrefix
  imps <- optionMaybe parseImplicits
  ty <- parseTypeSpec
  let imps' = case imps of
                Nothing -> []
                Just x -> x
  return (Decl var imps' ty)
  where
    parseImplicits :: YuParsec VarList
    parseImplicits = do
      _ <- yuKeyTok TokSquareL
      xs <- varListElem `sepBy1` yuKeyTok TokComma
      _ <- yuKeyTok TokSquareR
      return (concat xs)

    varListElem :: YuParsec [VarListElem]
    varListElem = do
      vars <- parseVarOrPrefix `sepBy` yuKeyTok TokSemiColon
      ty <- parseTypeSpec
      return (zip vars (repeat $ Just ty))

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
  lo <- yuKeyTok TokParenL
  xs <- varListElem `sepBy` yuKeyTok TokComma
  _ <- yuKeyTok TokParenR
  return (lo, concat xs)
  where
    varListElem :: YuParsec [VarListElem]
    varListElem = try varListElemMany <|> varListElem1

    varListElemMany :: YuParsec [VarListElem]
    varListElemMany = do
      vars <- parseVarOrPrefix `sepBy1` yuKeyTok TokSemiColon
      ty <- optionMaybe parseTypeSpec
      return (zip vars (repeat ty))

    varListElem1 :: YuParsec [VarListElem]
    varListElem1 = do
      var <- parseVarOrPrefix
      ty <- optionMaybe parseTypeSpec
      return [(var, ty)]

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
  lets <- many1 parseLetCase
  return (DefVal isPure d lets)

parseExternDef :: YuParsec Def
parseExternDef = do
  _ <- yuKeyTok TokExtern
  d <- parseDecl
  return (DefExtern d)

parseLetCase :: YuParsec LetCase
parseLetCase = do
  lo <- yuKeyTok TokLet
  (impls, args) <- parseLetPattern
  d <- optionMaybe letDef
  return (LetCase lo impls args d)
  where
    parseLetPattern ::
      YuParsec ([((Loc, String), ParsePattern)],
                Maybe [ParsePattern])
    parseLetPattern = do
      imps <- optionMaybe parseImplicits
      args <- optionMaybe parseArgs
      case imps of
        Nothing -> return ([], args)
        Just imps' -> return (imps', args)

    parseImplicits :: YuParsec [((Loc, String), ParsePattern)]
    parseImplicits = do
      _ <- yuKeyTok TokSquareL
      imps <- parseImplicit `sepBy1` yuKeyTok TokComma
      _ <- yuKeyTok TokSquareR
      return imps

    parseImplicit :: YuParsec ((Loc, String), ParsePattern)
    parseImplicit = do
      v <- yuVarTok
      _ <- yuKeyTok TokColonEq
      p <- parsePattern
      return (v, p)

    parseArgs :: YuParsec [ParsePattern]
    parseArgs = do
      _ <- yuKeyTok TokParenL
      args <- parsePattern `sepBy` yuKeyTok TokComma
      _ <- yuKeyTok TokParenR
      return args

    letDef :: YuParsec (Expr, OptWhereClause)
    letDef = do
      _ <- yuKeyTok TokEqGreater
      e <- parseExprSeq
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
parseDoExpr = tryParseDoExpr <|> parseFunExpr

tryParseDoExpr :: YuParsec Expr
tryParseDoExpr = try (yuKeyTok TokDo >> parseExprSeq)

parseFunExpr :: YuParsec Expr
parseFunExpr = tryParseFunExpr <|> parseArrowExpr

tryParseFunExpr :: YuParsec Expr
tryParseFunExpr = try doParseLazyFun <|> try doParseFun
  where
    doParseFun :: YuParsec Expr
    doParseFun = do
      (lo, xs) <- parseVarListLoc
      _ <- yuKeyTok TokPeriod
      e <- parseDoExpr
      return (ExprFun lo xs e)

    doParseLazyFun :: YuParsec Expr
    doParseLazyFun = do
      lo <- yuKeyTok TokSquareL
      _ <- yuKeyTok TokSquareR
      _ <- yuKeyTok TokPeriod
      e <- parseDoExpr
      return (ExprLazyFun lo e)

tryParseDoOrFun :: YuParsec Expr
tryParseDoOrFun = tryParseDoExpr <|> tryParseFunExpr

parseDoOrFunOr :: YuParsec Expr -> YuParsec Expr
parseDoOrFunOr e = try tryParseDoOrFun <|> e

parseArrowExpr :: YuParsec Expr
parseArrowExpr =
  try lazyParam <|> try zeroParams <|> try manyParams <|> try oneNamedParam <|> parseAppExprArrow
  where
    parseAppExprArrow :: YuParsec Expr
    parseAppExprArrow = do
      e <- parseOp6Expr
      a <- optionMaybe $ do
            x <- parseArrowSymbol
            y <- parseDoExpr
            return (x, y)
      case a of
        Nothing -> return e
        Just (b, a') -> return (ExprArrow (exprLoc e) b [Left e] a')

    lazyParam :: YuParsec Expr
    lazyParam = do
      lo <- yuKeyTok TokSquareL
      _ <- yuKeyTok TokSquareR
      b <- parseArrowSymbol
      a <- parseDoExpr
      return (ExprLazyArrow lo b a)

    zeroParams :: YuParsec Expr
    zeroParams = do
      lo <- yuKeyTok TokParenL
      _ <- yuKeyTok TokParenR
      b <- parseArrowSymbol
      a <- parseDoExpr
      return (ExprArrow lo b [] a)

    parseArrowSymbol :: YuParsec Bool
    parseArrowSymbol =
      (yuKeyTok TokDashGreater >> return False)
      <|> (yuKeyTok TokDashGreaterIo >> return True) 

    manyParams :: YuParsec Expr
    manyParams = do
      lo <- yuKeyTok TokParenL
      e <- paramElem
      _ <- yuKeyTok TokComma
      es <- paramElem `sepBy1` yuKeyTok TokComma
      _ <- yuKeyTok TokParenR
      b <- parseArrowSymbol
      a <- parseDoExpr
      return (ExprArrow lo b (e ++ concat es) a)

    oneNamedParam :: YuParsec Expr
    oneNamedParam = do
      lo <- yuKeyTok TokParenL
      e <- paramElemVar
      _ <- yuKeyTok TokParenR
      b <- parseArrowSymbol
      a <- parseDoExpr
      return (ExprArrow lo b e a)

    paramElem :: YuParsec [ExprListTypedElem]
    paramElem = try paramElemVar <|> paramElemExpr

    paramElemVar :: YuParsec [ExprListTypedElem]
    paramElemVar = do
      v <- parseVarOrPrefix `sepBy` yuKeyTok TokSemiColon
      ty <- parseTypeSpec
      return (zipWith (\x t -> Right (x, t)) v (repeat ty))

    paramElemExpr :: YuParsec [ExprListTypedElem]
    paramElemExpr = liftM (\x -> [Left x]) parseExpr

yuChainl1 :: YuParsec Expr -> YuParsec (Expr -> Expr -> Expr) -> YuParsec Expr
yuChainl1 p op = do { x <- p; rest x }
  where
    rest x = dorest x <|> return x
    dorest x = do
      f <- op
      y0 <- optionMaybe tryParseDoOrFun
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
      y0 <- optionMaybe tryParseDoOrFun
      case y0 of
        Nothing -> do { y <- scan; return (f x y) }
        Just y -> return (f x y)

parseOp6Expr :: YuParsec Expr
parseOp6Expr =
  parseOp5Expr `yuChainl1` op6
  where
    op6 :: YuParsec (Expr -> Expr -> Expr)
    op6 = do
      v <- yuOpTok 6
      return (\e1 e2 -> ExprApp (ExprVar v) [e1, e2])

parseOp5Expr :: YuParsec Expr
parseOp5Expr =
  parseOp4Expr `yuChainr1` op5
  where
    op5 :: YuParsec (Expr -> Expr -> Expr)
    op5 = do
      v <- yuOpTok 5
      return (\e1 e2 -> ExprApp (ExprVar v) [e1, e2])

parseOp4Expr :: YuParsec Expr
parseOp4Expr =
  parseOp3Expr `yuChainl1` op4
  where
    op4 :: YuParsec (Expr -> Expr -> Expr)
    op4 = do
      v <- yuOpTok 4
      return (\e1 e2 -> ExprApp (ExprVar v) [e1, e2])

parseOp3Expr :: YuParsec Expr
parseOp3Expr =
  parseOp2Expr `yuChainr1` op3
  where
    op3 :: YuParsec (Expr -> Expr -> Expr)
    op3 = do
      v <- yuOpTok 3
      return (\e1 e2 -> ExprApp (ExprVar v) [e1, e2])

parseOp2Expr :: YuParsec Expr
parseOp2Expr =
  parseOp1Expr `yuChainl1` op2
  where
    op2 :: YuParsec (Expr -> Expr -> Expr)
    op2 = do
      v <- yuOpTok 2
      return (\e1 e2 -> ExprApp (ExprVar v) [e1, e2])

parseOp1Expr :: YuParsec Expr
parseOp1Expr =
  parsePrefixOpExpr `yuChainr1` op1
  where
    op1 :: YuParsec (Expr -> Expr -> Expr)
    op1 = do
      v <- yuOpTok 1
      return (\e1 e2 -> ExprApp (ExprVar v) [e1, e2])

parsePrefixOpExpr :: YuParsec Expr
parsePrefixOpExpr = do
  v <- optionMaybe yuPreOpTok
  case v of
    Nothing -> parseAppExpr
    Just v' -> do
      e <- parseDoOrFunOr parsePrefixOpExpr
      return (ExprApp (ExprVar v') [e])

parseAppExpr :: YuParsec Expr
parseAppExpr = do
  e <- parseExprLeaf
  parseApp e
  where
    parseApp :: Expr -> YuParsec Expr
    parseApp e =
      try (parsePostfixDoOrFun e)
      <|> try (parsePostfixNormal e)
      <|> try (parseAppDoOrFun e)
      <|> try (parseAppNormal e)
      <|> try (parseAppSquare e)
      <|> return e

    appParen :: YuParsec [Expr]
    appParen = do
      _ <- yuKeyTok TokParenL
      args <- parseExprSeq `sepBy` yuKeyTok TokComma
      _ <- yuKeyTok TokParenR
      return args

    appSquare :: YuParsec [((Loc, String), Expr)]
    appSquare = do
      _ <- yuKeyTok TokSquareL
      args <- getNamedArg `sepBy` yuKeyTok TokComma
      _ <- yuKeyTok TokSquareR
      return args

    parsePostfixNormal :: Expr -> YuParsec Expr
    parsePostfixNormal e = do
      op <- yuPostfixOpTok
      imps <- optionMaybe appSquare
      args <- optionMaybe appParen
      let (f, args') = case args of
                        Nothing -> (id, [])
                        Just [] -> (\x -> ExprApp x [], [])
                        Just as -> (id, as)
      case imps of
        Nothing ->
          parseApp (f $ ExprApp (ExprVar op) (e : args'))
        Just [] ->
          if null args'
          then
            parseApp (f $ ExprLazyApp (ExprApp (ExprVar op) [e]))
          else
            parseApp (f $ ExprApp (ExprLazyApp (ExprApp (ExprVar op) [e])) args')
        Just imps' ->
          parseApp (f $ ExprApp (ExprImplicitApp (ExprVar op) imps') (e : args'))

    parsePostfixDoOrFun :: Expr -> YuParsec Expr
    parsePostfixDoOrFun e = do
      op <- yuPostfixOpTok
      imps <- optionMaybe appSquare
      a <- tryParseDoOrFun
      case imps of
        Nothing ->
          return (ExprApp (ExprVar op) [e, a])
        Just imps' -> 
          return (ExprApp (ExprImplicitApp (ExprVar op) imps') [e, a])

    parseAppNormal :: Expr -> YuParsec Expr
    parseAppNormal e = do
      args <- appParen
      parseApp (ExprApp e args)

    parseAppSquare :: Expr -> YuParsec Expr
    parseAppSquare e = do
      args <- appSquare
      if null args
        then parseApp (ExprLazyApp e)
        else parseApp (ExprImplicitApp e args)

    getNamedArg :: YuParsec ((Loc, String), Expr)
    getNamedArg = do
      v <- parseVarOrPrefix
      _ <- yuKeyTok TokColonEq
      t <- parseExpr
      return (v, t)

    parseAppDoOrFun :: Expr -> YuParsec Expr
    parseAppDoOrFun e = do
      a <- tryParseDoOrFun
      return (ExprApp e [a])

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
parseExprUnitElem = do
  lo <- yuKeyTok TokUnitElem
  return (ExprUnitElem lo)

parseExprUnitTy :: YuParsec Expr
parseExprUnitTy = do
  lo <- yuKeyTok TokUnitTy
  return (ExprUnitTy lo)

parseExprTy :: YuParsec Expr
parseExprTy = liftM ExprTy (yuKeyTok TokTy)

parseExprStr :: YuParsec Expr
parseExprStr = do
  (lo, s) <- yuStringLitTok
  return (ExprApp (ExprVar (lo, "mk.Str"))
            [yuCharsToExpr (stringToYuChars s) lo])
  where
    yuCharsToExpr :: [String] -> Loc -> Expr
    yuCharsToExpr [] lo = ExprVar (lo, "nil")
    yuCharsToExpr (c : cs) lo =
      ExprApp (ExprVar (lo, "_::_\\_List\\Ty"))
        [ExprVar (lo, c), yuCharsToExpr cs lo]

parseExprVar :: YuParsec Expr
parseExprVar = liftM ExprVar yuVarTok

parseExprSeq :: YuParsec Expr
parseExprSeq = do
  es <- exprOrAssign `sepBy1` yuKeyTok TokSemiColon
  let (start, [end]) = splitAt (length es - 1) es
  e <- case end of
         Right _ -> fail "expected expression"
         Left x -> return x
  return (foldr ExprSeq e start)
  where
    exprOrAssign :: YuParsec ExprSeqElem
    exprOrAssign = try exprElem <|> assignElem

    assignElem :: YuParsec ExprSeqElem
    assignElem = do
      p <- parsePattern
      _ <- yuKeyTok TokColonEq
      e <- parseExpr
      return (Right (p, e))

    exprElem :: YuParsec ExprSeqElem
    exprElem = do
      e <- parseExpr
      notFollowedBy (yuKeyTok TokColonEq)
      return (Left e)

parseParenExpr :: YuParsec Expr
parseParenExpr = do
  _ <- yuKeyTok TokParenL
  try parenPrefixOp <|> parenExpr
  where
    parenPrefixOp :: YuParsec Expr
    parenPrefixOp = do
      v <- yuPreOpTok'
      _ <- yuKeyTok TokParenR
      return (ExprVar v)

    parenExpr :: YuParsec Expr
    parenExpr = do
      e <- parseExprSeq
      _ <- yuKeyTok TokParenR
      return e

parseCaseExpr :: YuParsec Expr
parseCaseExpr = do
  lo <- yuKeyTok TokCase
  e <- parseExpr
  ofs <- many1 ofCase
  _ <- yuKeyTok TokEnd
  return (ExprCase lo e ofs)
  where
    ofCase :: YuParsec OfCase
    ofCase = do
      _ <- yuKeyTok TokOf
      p <- parsePattern
      e <- optionMaybe caseDef
      case e of
        Nothing -> return (Left p)
        Just e' -> return (Right (p, e'))

    caseDef :: YuParsec Expr
    caseDef = do
      _ <- yuKeyTok TokEqGreater
      e <- parseExprSeq
      return e

parsePattern :: YuParsec ParsePattern
parsePattern = parsePatternOp6

parsePatternOp6 :: YuParsec ParsePattern
parsePatternOp6 =
  parsePatternOp5 `chainl1` op6
  where
    op6 :: YuParsec (ParsePattern -> ParsePattern -> ParsePattern)
    op6 = do
      v <- yuOpTok 6
      return (\p1 p2 -> ParsePatternApp (ParsePatternVar v) [p1, p2])

parsePatternOp5 :: YuParsec ParsePattern
parsePatternOp5 =
  parsePatternOp4 `chainr1` op5
  where
    op5 :: YuParsec (ParsePattern -> ParsePattern -> ParsePattern)
    op5 = do
      v <- yuOpTok 5
      return (\p1 p2 -> ParsePatternApp (ParsePatternVar v) [p1, p2])

parsePatternOp4 :: YuParsec ParsePattern
parsePatternOp4 =
  parsePatternOp3 `chainl1` op4
  where
    op4 :: YuParsec (ParsePattern -> ParsePattern -> ParsePattern)
    op4 = do
      v <- yuOpTok 4
      return (\p1 p2 -> ParsePatternApp (ParsePatternVar v) [p1, p2])

parsePatternOp3 :: YuParsec ParsePattern
parsePatternOp3 =
  parsePatternOp2 `chainr1` op3
  where
    op3 :: YuParsec (ParsePattern -> ParsePattern -> ParsePattern)
    op3 = do
      v <- yuOpTok 3
      return (\p1 p2 -> ParsePatternApp (ParsePatternVar v) [p1, p2])

parsePatternOp2 :: YuParsec ParsePattern
parsePatternOp2 =
  parsePatternOp1 `chainl1` op2
  where
    op2 :: YuParsec (ParsePattern -> ParsePattern -> ParsePattern)
    op2 = do
      v <- yuOpTok 2
      return (\p1 p2 -> ParsePatternApp (ParsePatternVar v) [p1, p2])

parsePatternOp1 :: YuParsec ParsePattern
parsePatternOp1 =
  parsePatternPrefixOp `chainr1` op1
  where
    op1 :: YuParsec (ParsePattern -> ParsePattern -> ParsePattern)
    op1 = do
      v <- yuOpTok 1
      return (\p1 p2 -> ParsePatternApp (ParsePatternVar v) [p1, p2])

parsePatternPrefixOp :: YuParsec ParsePattern
parsePatternPrefixOp = do
  v <- optionMaybe yuPreOpTok
  case v of
    Nothing -> parsePatternApp
    Just v' -> do
      p <- parsePatternPrefixOp
      return (ParsePatternApp (ParsePatternVar v') [p])

parsePatternApp :: YuParsec ParsePattern
parsePatternApp = parsePatternLeaf >>= parseApp
  where
    parseApp :: ParsePattern -> YuParsec ParsePattern
    parseApp p =
      try (appPostfix p)
      <|> try (appParen >>= parseApp . ParsePatternApp p)
      <|> try (do
                args <- appSquare
                if null args
                  then parseApp (ParsePatternLazyApp p)
                  else parseApp (ParsePatternImplicitApp p args))
      <|> return p

    appParen :: YuParsec [ParsePattern]
    appParen = do
      _ <- yuKeyTok TokParenL
      args <- parsePattern `sepBy` yuKeyTok TokComma
      _ <- yuKeyTok TokParenR
      return args

    appSquare :: YuParsec [((Loc, String), ParsePattern)]
    appSquare = do
      _ <- yuKeyTok TokSquareL
      args <- parseArg `sepBy` yuKeyTok TokComma
      _ <- yuKeyTok TokSquareR
      return args

    appPostfix :: ParsePattern -> YuParsec ParsePattern
    appPostfix p = do
      op <- yuPostfixOpTok
      imps <- optionMaybe appSquare
      args <- optionMaybe appParen
      let (f, args') = case args of
                        Nothing -> (id, [])
                        Just [] -> (\x -> ParsePatternApp x [], [])
                        Just as -> (id, as)
      case imps of
        Nothing ->
          parseApp (f $ ParsePatternApp (ParsePatternVar op) (p : args'))
        Just [] ->
          if null args'
          then
            parseApp (f $ ParsePatternLazyApp
                          (ParsePatternApp (ParsePatternVar op) [p]))
          else
            parseApp (f $ ParsePatternApp
                          (ParsePatternLazyApp
                            (ParsePatternApp (ParsePatternVar op) [p])) args')
        Just imps' ->
          parseApp (f $ ParsePatternApp
                        (ParsePatternImplicitApp (ParsePatternVar op) imps') (p : args'))

    parseArg :: YuParsec ((Loc, String), ParsePattern)
    parseArg = do
      v <- yuVarTok
      _ <- yuKeyTok TokColonEq
      p <- parsePattern
      return (v, p)

parsePatternLeaf :: YuParsec ParsePattern
parsePatternLeaf = patStr <|> patUnit <|> patVar <|> patPat
  where
    patVar :: YuParsec ParsePattern
    patVar = liftM ParsePatternVar yuVarTok

    patPat :: YuParsec ParsePattern
    patPat = do
      lo <- yuKeyTok TokParenL
      try patPrefixOp <|> parenPat lo
      where
        patPrefixOp :: YuParsec ParsePattern
        patPrefixOp = do
          v <- yuPreOpTok'
          _ <- yuKeyTok TokParenR
          return (ParsePatternVar v)

        parenPat :: Loc -> YuParsec ParsePattern
        parenPat lo = do
          p <- optionMaybe parsePattern
          _ <- yuKeyTok TokParenR
          case p of
            Just p' -> return p'
            Nothing -> return (ParsePatternEmpty lo)

    patUnit :: YuParsec ParsePattern
    patUnit = do
      lo <- yuKeyTok TokUnitElem
      return (ParsePatternUnit lo)

    patStr :: YuParsec ParsePattern
    patStr = do
      (lo, s) <- yuStringLitTok
      return (ParsePatternApp (ParsePatternVar (lo, "mk.Str"))
                [yuCharsToPat (stringToYuChars s) lo])
      where
        yuCharsToPat :: [String] -> Loc -> ParsePattern
        yuCharsToPat [] lo = ParsePatternVar (lo, "nil")
        yuCharsToPat (c : cs) lo =
          ParsePatternApp (ParsePatternVar (lo, "_::_\\_List\\Ty"))
            [ParsePatternVar (lo, c), yuCharsToPat cs lo]

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
