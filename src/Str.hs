module Str
  ( quote
  , stdImportPath
  , stdRuntimePath
  , isKeyword
  , isStrictKeyword
  , isPunctuation
  , isOp1Char
  , isOp2Char
  , isOp3Char
  , isOp4Char
  , isOp5Char
  , isOp6Char
  , isGeneralWordChar
  , isOperatorChar
  , isOperator
  , isInfixOp
  , isInfixOp1
  , isInfixOp2
  , isInfixOp3
  , isInfixOp4
  , isInfixOp5
  , isInfixOp6
  , isPostfixOp
  , isPrefixOp
  , isRightAssocInfixOp
  , isLeftAssocInfixOp
  , stripOperatorStr
  , operandDelim
  , operandSplit
  , operandConcat
  , operandConcatMaybe
  , errorMsg
  , genMachineName
  , funcNameSep
  , anonymousFunString
  , funFromLazyString
  , anonymousDuplicateString
  , partialSpecializationString
  , unitName
  , underscoreName
  , implementationPrefix
  , infixOperatorPrefix
  , prefixOperatorPrefix
  , postfixOperatorPrefix
  , trivialCtorPrefix
  , papClosurePrefix
  , stringToYuChars
  )
where

import Loc
import Data.Char (ord, isAlphaNum)
import Control.Exception (assert)

quote :: String -> String
quote s = "‘" ++ s ++ "’"

stdImportPath :: FilePath
stdImportPath = "stdlib"

stdRuntimePath :: FilePath
stdRuntimePath = "runtime"

isKeyword :: String -> Bool
isKeyword s
  | s == "Ty" = True
  | isStrictKeyword s = True
  | True = False

isStrictKeyword :: String -> Bool
isStrictKeyword s
  | s == "import" = True
  | s == "extern" = True
  | s == "where" = True
  | s == "end" = True
  | s == "data" = True
  | s == "data.." = True
  | s == ":" = True
  | s == "let" = True
  | s == "val" = True
  | s == "val.." = True
  | s == ":=" = True
  | s == "->" = True
  | s == "->>" = True
  | s == "match" = True
  | s == "of" = True
  | s == "=>" = True
  | s == "&" = True
  | True = False

isPunctuation :: String -> Bool
isPunctuation s = s == "(" || s == ")" || s == "," || s == ";"

isGeneralWordChar :: Char -> Bool
isGeneralWordChar c = isAlphaNum c || elem c "'_.@!%^&*-=+~<>/?:|\\$"

isOperatorChar :: Char -> Bool
isOperatorChar c = elem c "@!%^&*-=+~<>/?:|$"

isOp1Char :: Char -> Bool
isOp1Char c = elem c "^@"

isOp2Char :: Char -> Bool
isOp2Char c = elem c "*/%"

isOp3Char :: Char -> Bool
isOp3Char c = elem c "$|&"

isOp4Char :: Char -> Bool
isOp4Char c = elem c "+-"

isOp5Char :: Char -> Bool
isOp5Char c = elem c "=:?!"

isOp6Char :: Char -> Bool
isOp6Char c = elem c "<>~"

isInfixOp :: String -> Bool
isInfixOp "_" = False
isInfixOp ('_' : c : _) = isOperatorChar c
isInfixOp _ = False

isRightAssocInfixOp :: String -> Bool
isRightAssocInfixOp s = isInfixOp5 s || isInfixOp3 s || isInfixOp1 s

isLeftAssocInfixOp :: String -> Bool
isLeftAssocInfixOp s = isInfixOp2 s || isInfixOp4 s || isInfixOp6 s

isInfixOp6 :: String -> Bool
isInfixOp6 s
  | isInfixOp s = assert (length s > 1) (isOp6Char (head (drop 1 s)))
  | True = False

isInfixOp5 :: String -> Bool
isInfixOp5 s
  | isInfixOp s = assert (length s > 1) (isOp5Char (head (drop 1 s)))
  | True = False

isInfixOp4 :: String -> Bool
isInfixOp4 s
  | isInfixOp s = assert (length s > 1) (isOp4Char (head (drop 1 s)))
  | True = False

isInfixOp3 :: String -> Bool
isInfixOp3 s
  | isInfixOp s = assert (length s > 1) (isOp3Char (head (drop 1 s)))
  | True = False

isInfixOp2 :: String -> Bool
isInfixOp2 s
  | isInfixOp s = assert (length s > 1) (isOp2Char (head (drop 1 s)))
  | True = False

isInfixOp1 :: String -> Bool
isInfixOp1 s
  | isInfixOp s = assert (length s > 1) (isOp1Char (head (drop 1 s)))
  | True = False

isPostfixOp :: String -> Bool
isPostfixOp "_" = False
isPostfixOp ('_' : x : _) = not (isOperatorChar x)
isPostfixOp _ = False

isPrefixOp :: String -> Bool
isPrefixOp s = isOperatorChar (head s)

isOperator :: String -> Bool
isOperator na = isPostfixOp na || isInfixOp na || isPrefixOp na

operandSplit :: String -> (String, Maybe String)
operandSplit "" = ("", Nothing)
operandSplit ('\\' : ys) = ("", Just ys)
operandSplit (x : xs) = let (xs', ys) = operandSplit xs in (x : xs', ys)

operandConcatMaybe :: String -> Maybe String -> String
operandConcatMaybe s1 Nothing = s1
operandConcatMaybe s1 (Just s2) = operandConcat s1 s2

operandConcat :: String -> String -> String
operandConcat s1 s2 = s1 ++ '\\' : s2

stripOperatorStr :: String -> String
stripOperatorStr = filter (\x -> not (x `elem` "_"))

errorMsg :: FilePath -> Loc -> String -> String
errorMsg p lo s = p ++ ":" ++ show lo ++ ": error: " ++ s

genMachineName :: String -> String
genMachineName s = "yu_" ++ doGenMachineName s

doGenMachineName :: String -> String
doGenMachineName "" = ""
doGenMachineName (c : s) = genChar ++ doGenMachineName s
  where
    genChar :: String
    genChar =
      case c of
        '.' -> "_do"
        '\'' -> "_si"
        '@' -> "_at"
        '$' -> "_Do"
        '!' -> "_ex"
        '%' -> "_pe"
        '^' -> "_ca"
        '&' -> "_am"
        '*' -> "_as"
        '-' -> "_mi"
        '=' -> "_eq"
        '+' -> "_pl"
        '~' -> "_ti"
        '<' -> "_le"
        '>' -> "_gr"
        '/' -> "_sl"
        '\\' -> operandDelim
        '?' -> "_qu"
        ':' -> "_co"
        '|' -> "_ve"
        _ -> c : ""

operandDelim :: String
operandDelim = "_ba"

funcNameSep :: String
funcNameSep = "___"

-- moduleNameSep = "__"

anonymousFunString :: String
anonymousFunString = "_fn"

funFromLazyString :: String
funFromLazyString = "_fz"

anonymousDuplicateString :: String
anonymousDuplicateString = "_du"

partialSpecializationString :: Int -> Int -> String
partialSpecializationString i j =
  "_ps" ++ show i ++ "_" ++ show j ++ "_"

unitName :: String
unitName = "_un"

underscoreName :: String
underscoreName = "_Un"

implementationPrefix :: String
implementationPrefix = "_im"

trivialCtorPrefix :: String
trivialCtorPrefix = "_ct"

papClosurePrefix :: String
papClosurePrefix = "_pa"

infixOperatorPrefix :: String
infixOperatorPrefix = "_in"

prefixOperatorPrefix :: String
prefixOperatorPrefix = "_pr"

postfixOperatorPrefix :: String
postfixOperatorPrefix = "_po"

stringToYuChars :: String -> [String]
stringToYuChars "" = []
stringToYuChars ('\\' : s) = escapedStringToYuChars s
stringToYuChars (c : s) = charToYuChar c : stringToYuChars s

escapedStringToYuChars :: String -> [String]
escapedStringToYuChars ('\\' : s) = charToYuChar '\\' : stringToYuChars s
escapedStringToYuChars ('t' : s) = charToYuChar '\t' : stringToYuChars s
escapedStringToYuChars ('n' : s) = charToYuChar '\n' : stringToYuChars s
escapedStringToYuChars ('"' : s) = charToYuChar '"' : stringToYuChars s
escapedStringToYuChars ('x' : d1 : d2 : s) =
  hexToYuChar d1 d2 : stringToYuChars s
escapedStringToYuChars s =
  error $ "unexpected escaped sequence " ++ quote ('\\' : s)

charToYuChar :: Char -> String
charToYuChar = numToYuChar . ord

hexToYuChar :: Char -> Char -> String
hexToYuChar d1 d2 = numToYuChar (read ("0x" ++ [d1, d2]))

numToYuChar :: Int -> String
numToYuChar 0 = "'nul"
numToYuChar 1 = "'soh"
numToYuChar 2 = "'stx"
numToYuChar 3 = "'etx"
numToYuChar 4 = "'eot"
numToYuChar 5 = "'enq"
numToYuChar 6 = "'ack"
numToYuChar 7 = "'bel"
numToYuChar 8 = "'bs"
numToYuChar 9 = "'tab"
numToYuChar 10 = "'lf"
numToYuChar 11 = "'vt"
numToYuChar 12 = "'ff"
numToYuChar 13 = "'cr"
numToYuChar 14 = "'so"
numToYuChar 15 = "'si"
numToYuChar 16 = "'dle"
numToYuChar 17 = "'dc1"
numToYuChar 18 = "'dc2"
numToYuChar 19 = "'dc3"
numToYuChar 20 = "'dc4"
numToYuChar 21 = "'nak"
numToYuChar 22 = "'syn"
numToYuChar 23 = "'etb"
numToYuChar 24 = "'can"
numToYuChar 25 = "'em"
numToYuChar 26 = "'sub"
numToYuChar 27 = "'esc"
numToYuChar 28 = "'fs"
numToYuChar 29 = "'gs"
numToYuChar 30 = "'rs"
numToYuChar 31 = "'us"
numToYuChar 32 = "'sp"
numToYuChar 33 = "'!"
numToYuChar 34 = "'qu"
numToYuChar 35 = "'po"
numToYuChar 36 = "'$"
numToYuChar 37 = "'%"
numToYuChar 38 = "'&"
numToYuChar 39 = "''"
numToYuChar 40 = "'lpa"
numToYuChar 41 = "'rpa"
numToYuChar 42 = "'*"
numToYuChar 43 = "'+"
numToYuChar 44 = "'co"
numToYuChar 45 = "'-"
numToYuChar 46 = "'."
numToYuChar 47 = "'/"
numToYuChar 48 = "'0"
numToYuChar 49 = "'1"
numToYuChar 50 = "'2"
numToYuChar 51 = "'3"
numToYuChar 52 = "'4"
numToYuChar 53 = "'5"
numToYuChar 54 = "'6"
numToYuChar 55 = "'7"
numToYuChar 56 = "'8"
numToYuChar 57 = "'9"
numToYuChar 58 = "':"
numToYuChar 59 = "'se"
numToYuChar 60 = "'<"
numToYuChar 61 = "'="
numToYuChar 62 = "'>"
numToYuChar 63 = "'?"
numToYuChar 64 = "'@"
numToYuChar 65 = "'A"
numToYuChar 66 = "'B"
numToYuChar 67 = "'C"
numToYuChar 68 = "'D"
numToYuChar 69 = "'E"
numToYuChar 70 = "'F"
numToYuChar 71 = "'G"
numToYuChar 72 = "'H"
numToYuChar 73 = "'I"
numToYuChar 74 = "'J"
numToYuChar 75 = "'K"
numToYuChar 76 = "'L"
numToYuChar 77 = "'M"
numToYuChar 78 = "'N"
numToYuChar 79 = "'O"
numToYuChar 80 = "'P"
numToYuChar 81 = "'Q"
numToYuChar 82 = "'R"
numToYuChar 83 = "'S"
numToYuChar 84 = "'T"
numToYuChar 85 = "'U"
numToYuChar 86 = "'V"
numToYuChar 87 = "'W"
numToYuChar 88 = "'X"
numToYuChar 89 = "'Y"
numToYuChar 90 = "'Z"
numToYuChar 91 = "'lsq"
numToYuChar 92 = "'ba"
numToYuChar 93 = "'rsq"
numToYuChar 94 = "'^"
numToYuChar 95 = "'un"
numToYuChar 96 = "'gr"
numToYuChar 97 = "'a"
numToYuChar 98 = "'b"
numToYuChar 99 = "'c"
numToYuChar 100 = "'d"
numToYuChar 101 = "'e"
numToYuChar 102 = "'f"
numToYuChar 103 = "'g"
numToYuChar 104 = "'h"
numToYuChar 105 = "'i"
numToYuChar 106 = "'j"
numToYuChar 107 = "'k"
numToYuChar 108 = "'l"
numToYuChar 109 = "'m"
numToYuChar 110 = "'n"
numToYuChar 111 = "'o"
numToYuChar 112 = "'p"
numToYuChar 113 = "'q"
numToYuChar 114 = "'r"
numToYuChar 115 = "'s"
numToYuChar 116 = "'t"
numToYuChar 117 = "'u"
numToYuChar 118 = "'v"
numToYuChar 119 = "'w"
numToYuChar 120 = "'x"
numToYuChar 121 = "'y"
numToYuChar 122 = "'z"
numToYuChar 123 = "'lcu"
numToYuChar 124 = "'|"
numToYuChar 125 = "'rcu"
numToYuChar 126 = "'~"
numToYuChar 127 = "'del"
numToYuChar n = error $ "unexpected char number " ++ show n
