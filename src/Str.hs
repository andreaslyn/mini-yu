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
  , isFixOp
  , isFixOp1
  , isFixOp2
  , isFixOp3
  , isFixOp4
  , isFixOp5
  , isFixOp6
  , isPostfixOp
  , isRightAssocFixOp
  , isLeftAssocFixOp
  , varNameModuleSplit
  , moduleSplit
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
  , trivialCtorPrefix
  , papClosurePrefix
  , stringToYuChars
  )
where

import Loc
import Data.Char (ord, isAlphaNum)

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
  | s == "export" = True
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
  | s == "|" = True
  | True = False

isPunctuation :: String -> Bool
isPunctuation s = s == "(" || s == ")" || s == "," || s == ";"

isGeneralWordChar :: Char -> Bool
isGeneralWordChar c = isAlphaNum c || elem c "'_.@!%^&*-=+~<>/?:|#$"

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

isFixOp :: String -> Bool
isFixOp (c : _) = isOperatorChar c
isFixOp _ = False

isRightAssocFixOp :: String -> Bool
isRightAssocFixOp s = isFixOp5 s || isFixOp3 s || isFixOp1 s

isLeftAssocFixOp :: String -> Bool
isLeftAssocFixOp s = isFixOp2 s || isFixOp4 s || isFixOp6 s

isFixOp6 :: String -> Bool
isFixOp6 s
  | isFixOp s = isOp6Char (head s)
  | True = False

isFixOp5 :: String -> Bool
isFixOp5 s
  | isFixOp s = isOp5Char (head s)
  | True = False

isFixOp4 :: String -> Bool
isFixOp4 s
  | isFixOp s = isOp4Char (head s)
  | True = False

isFixOp3 :: String -> Bool
isFixOp3 s
  | isFixOp s = isOp3Char (head s)
  | True = False

isFixOp2 :: String -> Bool
isFixOp2 s
  | isFixOp s = isOp2Char (head s)
  | True = False

isFixOp1 :: String -> Bool
isFixOp1 s
  | isFixOp s = isOp1Char (head s)
  | True = False

isPostfixOp :: String -> Bool
isPostfixOp ('.' : _) = True
isPostfixOp _ = False

isOperator :: String -> Bool
isOperator na = isPostfixOp na || isFixOp na

doVarNameModuleSplit :: String -> (String, Maybe String)
doVarNameModuleSplit vn =
  let (v1, w2) = span (/= '.') vn
  in case w2 of
      _ : v2 -> (v1, Just v2)
      "" -> (v1, Nothing)

varNameModuleSplit :: String -> (String, Maybe String)
varNameModuleSplit ('.' : vn) =
  let (v1, v2) = doVarNameModuleSplit vn
  in ('.' : v1, v2)
varNameModuleSplit vn = doVarNameModuleSplit vn

doModuleSplit :: String -> (String, Maybe String)
doModuleSplit s0 =
  let (s, t0) = operandSplit s0
      t = case t0 of
            Nothing -> ""
            Just t' -> '#' : t'
      (x, y) = span (/='.') s
  in case y of
      "" -> (x ++ t, Nothing)
      _ : y' -> (x ++ t, Just y')

moduleSplit :: String -> (String, Maybe String)
moduleSplit ('.' : s) =
  let (x, y) = doModuleSplit s in ('.' : x, y)
moduleSplit s =
  doModuleSplit s

operandSplit :: String -> (String, Maybe String)
operandSplit "" = ("", Nothing)
operandSplit ('#' : ys) = ("", Just ys)
operandSplit (x : xs) = let (xs', ys) = operandSplit xs in (x : xs', ys)

operandConcatMaybe :: String -> Maybe String -> String
operandConcatMaybe s1 Nothing = s1
operandConcatMaybe s1 (Just s2) = operandConcat s1 s2

operandConcat :: String -> String -> String
operandConcat s1 s2 = s1 ++ '#' : s2

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
        '\\' -> error "unexpected backslash in identifier" -- "_ba"
        '?' -> "_qu"
        ':' -> "_co"
        '|' -> "_ve"
        '#' -> operandDelim
        _ -> c : ""

operandDelim :: String
operandDelim = "_nu"

funcNameSep :: String
funcNameSep = "__"

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
