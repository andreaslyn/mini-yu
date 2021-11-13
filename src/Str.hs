module Str
  ( quote
  , stdImportPath
  , stdRuntimePath
  , outputFolderName
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

import Loc (Loc)
import Data.Char (ord, isAlphaNum)

quote :: String -> String
quote s = "‘" ++ s ++ "’"

stdImportPath :: FilePath
stdImportPath = "stdlib"

stdRuntimePath :: FilePath
stdRuntimePath = "runtime"

outputFolderName :: FilePath
outputFolderName = ".yupackage"

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
moduleSplit s = doModuleSplit s

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
numToYuChar 0 = "'nul.yu/Char/Type"
numToYuChar 1 = "'soh.yu/Char/Type"
numToYuChar 2 = "'stx.yu/Char/Type"
numToYuChar 3 = "'etx.yu/Char/Type"
numToYuChar 4 = "'eot.yu/Char/Type"
numToYuChar 5 = "'enq.yu/Char/Type"
numToYuChar 6 = "'ack.yu/Char/Type"
numToYuChar 7 = "'bel.yu/Char/Type"
numToYuChar 8 = "'bs.yu/Char/Type"
numToYuChar 9 = "'tab.yu/Char/Type"
numToYuChar 10 = "'lf.yu/Char/Type"
numToYuChar 11 = "'vt.yu/Char/Type"
numToYuChar 12 = "'ff.yu/Char/Type"
numToYuChar 13 = "'cr.yu/Char/Type"
numToYuChar 14 = "'so.yu/Char/Type"
numToYuChar 15 = "'si.yu/Char/Type"
numToYuChar 16 = "'dle.yu/Char/Type"
numToYuChar 17 = "'dc1.yu/Char/Type"
numToYuChar 18 = "'dc2.yu/Char/Type"
numToYuChar 19 = "'dc3.yu/Char/Type"
numToYuChar 20 = "'dc4.yu/Char/Type"
numToYuChar 21 = "'nak.yu/Char/Type"
numToYuChar 22 = "'syn.yu/Char/Type"
numToYuChar 23 = "'etb.yu/Char/Type"
numToYuChar 24 = "'can.yu/Char/Type"
numToYuChar 25 = "'em.yu/Char/Type"
numToYuChar 26 = "'sub.yu/Char/Type"
numToYuChar 27 = "'esc.yu/Char/Type"
numToYuChar 28 = "'fs.yu/Char/Type"
numToYuChar 29 = "'gs.yu/Char/Type"
numToYuChar 30 = "'rs.yu/Char/Type"
numToYuChar 31 = "'us.yu/Char/Type"
numToYuChar 32 = "'sp.yu/Char/Type"
numToYuChar 33 = "'!.yu/Char/Type"
numToYuChar 34 = "'qu.yu/Char/Type"
numToYuChar 35 = "'po.yu/Char/Type"
numToYuChar 36 = "'$.yu/Char/Type"
numToYuChar 37 = "'%.yu/Char/Type"
numToYuChar 38 = "'&.yu/Char/Type"
numToYuChar 39 = "''.yu/Char/Type"
numToYuChar 40 = "'lpa.yu/Char/Type"
numToYuChar 41 = "'rpa.yu/Char/Type"
numToYuChar 42 = "'*.yu/Char/Type"
numToYuChar 43 = "'+.yu/Char/Type"
numToYuChar 44 = "'co.yu/Char/Type"
numToYuChar 45 = "'-.yu/Char/Type"
numToYuChar 46 = "'pe.yu/Char/Type"
numToYuChar 47 = "'/.yu/Char/Type"
numToYuChar 48 = "'0.yu/Char/Type"
numToYuChar 49 = "'1.yu/Char/Type"
numToYuChar 50 = "'2.yu/Char/Type"
numToYuChar 51 = "'3.yu/Char/Type"
numToYuChar 52 = "'4.yu/Char/Type"
numToYuChar 53 = "'5.yu/Char/Type"
numToYuChar 54 = "'6.yu/Char/Type"
numToYuChar 55 = "'7.yu/Char/Type"
numToYuChar 56 = "'8.yu/Char/Type"
numToYuChar 57 = "'9.yu/Char/Type"
numToYuChar 58 = "':.yu/Char/Type"
numToYuChar 59 = "'se.yu/Char/Type"
numToYuChar 60 = "'<.yu/Char/Type"
numToYuChar 61 = "'=.yu/Char/Type"
numToYuChar 62 = "'>.yu/Char/Type"
numToYuChar 63 = "'?.yu/Char/Type"
numToYuChar 64 = "'@.yu/Char/Type"
numToYuChar 65 = "'A.yu/Char/Type"
numToYuChar 66 = "'B.yu/Char/Type"
numToYuChar 67 = "'C.yu/Char/Type"
numToYuChar 68 = "'D.yu/Char/Type"
numToYuChar 69 = "'E.yu/Char/Type"
numToYuChar 70 = "'F.yu/Char/Type"
numToYuChar 71 = "'G.yu/Char/Type"
numToYuChar 72 = "'H.yu/Char/Type"
numToYuChar 73 = "'I.yu/Char/Type"
numToYuChar 74 = "'J.yu/Char/Type"
numToYuChar 75 = "'K.yu/Char/Type"
numToYuChar 76 = "'L.yu/Char/Type"
numToYuChar 77 = "'M.yu/Char/Type"
numToYuChar 78 = "'N.yu/Char/Type"
numToYuChar 79 = "'O.yu/Char/Type"
numToYuChar 80 = "'P.yu/Char/Type"
numToYuChar 81 = "'Q.yu/Char/Type"
numToYuChar 82 = "'R.yu/Char/Type"
numToYuChar 83 = "'S.yu/Char/Type"
numToYuChar 84 = "'T.yu/Char/Type"
numToYuChar 85 = "'U.yu/Char/Type"
numToYuChar 86 = "'V.yu/Char/Type"
numToYuChar 87 = "'W.yu/Char/Type"
numToYuChar 88 = "'X.yu/Char/Type"
numToYuChar 89 = "'Y.yu/Char/Type"
numToYuChar 90 = "'Z.yu/Char/Type"
numToYuChar 91 = "'lsq.yu/Char/Type"
numToYuChar 92 = "'ba.yu/Char/Type"
numToYuChar 93 = "'rsq.yu/Char/Type"
numToYuChar 94 = "'^.yu/Char/Type"
numToYuChar 95 = "'un.yu/Char/Type"
numToYuChar 96 = "'gr.yu/Char/Type"
numToYuChar 97 = "'a.yu/Char/Type"
numToYuChar 98 = "'b.yu/Char/Type"
numToYuChar 99 = "'c.yu/Char/Type"
numToYuChar 100 = "'d.yu/Char/Type"
numToYuChar 101 = "'e.yu/Char/Type"
numToYuChar 102 = "'f.yu/Char/Type"
numToYuChar 103 = "'g.yu/Char/Type"
numToYuChar 104 = "'h.yu/Char/Type"
numToYuChar 105 = "'i.yu/Char/Type"
numToYuChar 106 = "'j.yu/Char/Type"
numToYuChar 107 = "'k.yu/Char/Type"
numToYuChar 108 = "'l.yu/Char/Type"
numToYuChar 109 = "'m.yu/Char/Type"
numToYuChar 110 = "'n.yu/Char/Type"
numToYuChar 111 = "'o.yu/Char/Type"
numToYuChar 112 = "'p.yu/Char/Type"
numToYuChar 113 = "'q.yu/Char/Type"
numToYuChar 114 = "'r.yu/Char/Type"
numToYuChar 115 = "'s.yu/Char/Type"
numToYuChar 116 = "'t.yu/Char/Type"
numToYuChar 117 = "'u.yu/Char/Type"
numToYuChar 118 = "'v.yu/Char/Type"
numToYuChar 119 = "'w.yu/Char/Type"
numToYuChar 120 = "'x.yu/Char/Type"
numToYuChar 121 = "'y.yu/Char/Type"
numToYuChar 122 = "'z.yu/Char/Type"
numToYuChar 123 = "'lcu.yu/Char/Type"
numToYuChar 124 = "'|.yu/Char/Type"
numToYuChar 125 = "'rcu.yu/Char/Type"
numToYuChar 126 = "'~.yu/Char/Type"
numToYuChar 127 = "'del.yu/Char/Type"
numToYuChar n = error $ "unexpected char number " ++ show n
