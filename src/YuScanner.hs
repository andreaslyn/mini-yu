module YuScanner
  ( TokType (..)
  , tokTypeString
  , tokTypeLength
  , Tok
  , tok
  , tokType
  , tokLoc
  , tokEndLoc
  , runScan
  )
where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isPrint)
import Scanner (Scanner, scanOnly, anyChar8, lookAheadChar8)
import Control.Monad
import Loc
import Str (quote, errorMsg, isOp1Char, isOp2Char, isOp3Char,
            isOp4Char, isOp5Char, isOp6Char, isGeneralWordChar)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class

data TokType =
    TokVar String
  | TokPostfixOp String
  | TokStringLit String
  | TokOp1 String --  ^ @        (right associative)
  | TokOp2 String --  * / %      (left associative)
  | TokOp3 String --  $ | &      (right associative)
  | TokOp4 String --  + -        (left associative)
  | TokOp5 String --  = : ? !    (right associative)
  | TokOp6 String --  < > ~      (left associative)
  | TokDo
  | TokImport
  | TokWhere
  | TokEnd
  | TokData
  | TokDataDotDot
  | TokLet
  | TokExtern
  | TokVal
  | TokValDotDot
  | TokCase
  | TokOf
  | TokTy
  | TokUnitTy
  | TokUnitElem
  | TokPeriod
  | TokAmp
  | TokColon
  | TokColonEq
  | TokDashGreater
  | TokDashGreaterIo
  | TokEqGreater
  | TokParenL
  | TokParenR
  | TokSquareL
  | TokSquareR
  | TokCurlyL
  | TokCurlyR
  | TokComma
  | TokSemiColon
  | TokEof
  deriving Eq

data Tok = Tok TokType Loc

tokLoc :: Tok -> Loc
tokLoc (Tok _ lo) = lo

tokType :: Tok -> TokType
tokType (Tok t _) = t

tok :: TokType -> Loc -> Tok
tok = Tok

tokTypeString :: TokType -> String
tokTypeString t =
  case t of
    TokVar s -> s
    TokPostfixOp s -> s
    TokOp1 s -> s
    TokOp2 s -> s
    TokOp3 s -> s
    TokOp4 s -> s
    TokOp5 s -> s
    TokOp6 s -> s
    TokStringLit s -> "\"" ++ s ++ "\""
    TokDo -> "do"
    TokImport -> "import"
    TokWhere -> "where"
    TokEnd -> "end"
    TokData -> "data"
    TokDataDotDot -> "data.."
    TokLet -> "let"
    TokExtern -> "extern"
    TokVal -> "val"
    TokValDotDot -> "val.."
    TokCase -> "case"
    TokOf -> "of"
    TokTy -> "Ty"
    TokUnitTy -> "Unit"
    TokUnitElem -> "unit"
    TokPeriod -> "."
    TokAmp -> "&"
    TokColon -> ":"
    TokColonEq -> ":="
    TokDashGreater -> "->"
    TokDashGreaterIo -> "->>"
    TokEqGreater -> "=>"
    TokParenL -> "("
    TokParenR -> ")"
    TokSquareL -> "["
    TokSquareR -> "]"
    TokCurlyL -> "{"
    TokCurlyR -> "}"
    TokComma -> ","
    TokSemiColon -> ";"
    TokEof -> "end of file"

tokTypeLength :: TokType -> Int
tokTypeLength = length . tokTypeString

tokEndLoc :: Tok -> Loc
tokEndLoc (Tok t lo) = loc (line lo) (column lo + tokTypeLength t)

instance Show TokType where
  show = quote . tokTypeString

instance Show Tok where
  show (Tok t lo) = show t ++ ":" ++ show lo

type SState = (FilePath, Loc, [Tok])
type SScanner a = StateT SState Scanner a

prFilePath :: SState -> FilePath
prFilePath (p, _, _) = p

prLoc :: SState -> Loc
prLoc (_, lo, _) = lo

prToks :: SState -> [Tok]
prToks (_, _, ts) = ts

getFilePath :: SScanner FilePath
getFilePath = liftM prFilePath get

getLoc :: SScanner Loc
getLoc = liftM prLoc get

getToks :: SScanner [Tok]
getToks = liftM prToks get

putTok :: Tok -> SScanner ()
putTok t = do
  p <- getFilePath
  lo <- getLoc
  ts <- getToks
  put (p, lo, t : ts)

putLoc :: Loc -> SScanner ()
putLoc lo = do
  p <- getFilePath
  ts <- getToks
  put (p, lo, ts)

tryPeekChar :: SScanner (Maybe Char)
tryPeekChar = lift lookAheadChar8

skipChar :: SScanner ()
skipChar = do
  c <- lift anyChar8
  lo <- getLoc
  if c == '\n'
    then putLoc (loc (line lo + 1) 1)
    else if isPrint c
      then putLoc (loc (line lo) (column lo + 1))
      else return ()

trySkipChar :: SScanner ()
trySkipChar = do
  c <- tryPeekChar
  case c of
    Nothing -> return ()
    Just _ -> skipChar

tryConsumeChar :: SScanner (Maybe Char)
tryConsumeChar = do
  c <- tryPeekChar
  case c of
    Nothing -> return Nothing
    Just c' -> do
      skipChar
      return (Just c')

consumeChar :: SScanner Char
consumeChar = do
  c <- tryPeekChar
  case c of
    Nothing -> failHere "unexpected end of file"
    Just c' -> do
      skipChar
      return c'

failLoc :: String -> Loc -> SScanner a
failLoc msg lo = do
  p <- getFilePath
  fail (errorMsg p lo msg)

failHere :: String -> SScanner a
failHere msg = getLoc >>= failLoc msg

makeWordTok :: String -> Loc -> SScanner Tok
makeWordTok s lo
  | s == "do" = return (tok TokDo lo)
  | s == "import" = return (tok TokImport lo)
  | s == "where" = return (tok TokWhere lo)
  | s == "end" = return (tok TokEnd lo)
  | s == "data" = return (tok TokData lo)
  | s == "data.." = return (tok TokDataDotDot lo)
  | s == ":" = return (tok TokColon lo)
  | s == "let" = return (tok TokLet lo)
  | s == "extern" = return (tok TokExtern lo)
  | s == "val" = return (tok TokVal lo)
  | s == "val.." = return (tok TokValDotDot lo)
  | s == ":=" = return (tok TokColonEq lo)
  | s == "Ty" = return (tok TokTy lo)
  | s == "Unit" = return (tok TokUnitTy lo)
  | s == "unit" = return (tok TokUnitElem lo)
  | s == "." = return (tok TokPeriod lo)
  | s == "&" = return (tok TokAmp lo)
  | s == "->" = return (tok TokDashGreater lo)
  | s == "->>" = return (tok TokDashGreaterIo lo)
  | s == "case" = return (tok TokCase lo)
  | s == "of" = return (tok TokOf lo)
  | s == "=>" = return (tok TokEqGreater lo)
  | isOp1Char (head s) = return (tok (TokOp1 s) lo)
  | isOp2Char (head s) = return (tok (TokOp2 s) lo)
  | isOp3Char (head s) = return (tok (TokOp3 s) lo)
  | isOp4Char (head s) = return (tok (TokOp4 s) lo)
  | isOp5Char (head s) = return (tok (TokOp5 s) lo)
  | isOp6Char (head s) = return (tok (TokOp6 s) lo)
  | head s == '.' = return (tok (TokPostfixOp s) lo)
  | otherwise = return (tok (TokVar s) lo)

skipRestOfLine :: SScanner ()
skipRestOfLine = do
  c <- tryPeekChar
  if c == Nothing || c == Just '\n'
    then trySkipChar
    else skipChar >> skipRestOfLine

skipRestOfBlockComment :: SScanner ()
skipRestOfBlockComment = skipNest 1
  where
    skipNest :: Int -> SScanner ()
    skipNest 0 = return ()
    skipNest i = do
      c <- consumeChar
      if c /= '#'
        then skipNest i
        else consumeChar >>= skipNestHashtag i

    skipNestHashtag :: Int -> Char -> SScanner ()
    skipNestHashtag i c
      | c == '{' = skipNest (i + 1)
      | c == '}' = skipNest (i - 1)
      | otherwise = skipNest i

tryNewTok :: Char -> Loc -> SScanner (Maybe Tok)
tryNewTok c lo
  | c `elem` "\t\n\r " = return Nothing
  | isGeneralWordChar c = do
      w <- readWord
      t <- makeWordTok (c:w) lo
      return (Just t)
  | c == '"' = liftM Just readStringLit
  | c == '#' = readComment >> return Nothing
  | c == '(' = return $ Just (tok TokParenL lo)
  | c == ')' = return $ Just (tok TokParenR lo)
  | c == '[' = return $ Just (tok TokSquareL lo)
  | c == ']' = return $ Just (tok TokSquareR lo)
  | c == '{' = return $ Just (tok TokCurlyL lo)
  | c == '}' = return $ Just (tok TokCurlyR lo)
  | c == ',' = return $ Just (tok TokComma lo)
  | c == ';' = return $ Just (tok TokSemiColon lo)
  | otherwise = failLoc ("invalid input character") lo
  where
    readWord :: SScanner String
    readWord = do
      x <- tryPeekChar
      case x of
        Nothing -> return ""
        Just c' ->
          if isGeneralWordChar c'
            then skipChar >> readWord >>= return . (c':)
            else return ""

    readStringLit :: SScanner Tok
    readStringLit = do
      s <- doReadString
      return (tok (TokStringLit s) lo)

    doReadString :: SScanner String
    doReadString = do
      c' <- consumeChar
      if c' == '"'
        then return ""
        else do
              r <- if c' == '\\' then readEscapedString else return [c']
              t <- doReadString
              return (r ++ t)

    checkIsOctDigit :: Loc -> Char -> SScanner ()
    checkIsOctDigit lo' c'
      | c' `elem` "01234567" = return ()
      | True = failLoc ("expected octal digit 0-7, but got " ++ quote [c']) lo'

    checkIsHexDigit :: Loc -> Char -> SScanner ()
    checkIsHexDigit lo' c'
      | c' `elem` "0123456789ABCDEF" = return ()
      | True = failLoc ("expected hexadecimal digit 0-F, but got " ++ quote [c']) lo'

    readEscapedString :: SScanner String
    readEscapedString = do
      lo' <- getLoc
      c' <- consumeChar
      if c' == '\\' || c' == 'n' || c' == 't' || c' == '"'
      then return ['\\', c']
      else
        if c' == 'x'
        then do
          lo1 <- getLoc
          d1 <- consumeChar
          checkIsOctDigit lo1 d1
          lo2 <- getLoc
          d2 <- consumeChar
          checkIsHexDigit lo2 d2
          return ['\\', 'x', d1, d2]
        else
          failLoc ("unexpected escape charatcer " ++ quote [c']) lo'

    readComment :: SScanner ()
    readComment = do
      c' <- consumeChar
      if c' == '#'
        then skipRestOfLine
        else if c' == '{'
          then skipRestOfBlockComment
          else failHere ("unexpected character " ++ quote [c'] ++
                ", expected " ++ quote "#" ++ " or " ++ quote "{")

tryConsumeNextTok :: SScanner Tok
tryConsumeNextTok = do
  lo <- getLoc
  c <- tryConsumeChar
  case c of
    Nothing -> return (tok TokEof lo)
    Just c' -> do
      t <- tryNewTok c' lo
      case t of
        Nothing -> tryConsumeNextTok
        Just t' -> return t'

scanLoop :: SScanner ()
scanLoop = do
  t <- tryConsumeNextTok
  putTok t
  if tokType t == TokEof
    then return ()
    else scanLoop

startScan :: FilePath -> Scanner [Tok]
startScan p =
  liftM (reverse . prToks . snd) (runStateT scanLoop (p, loc 1 1, []))

runScan' :: FilePath -> ByteString -> Either String [Tok]
runScan' p = scanOnly (startScan p)

runScan :: MonadIO m => FilePath -> m (Either String [Tok])
runScan p = fmap (runScan' p) (liftIO (BS.readFile p))
