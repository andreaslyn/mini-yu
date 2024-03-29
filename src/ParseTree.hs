module ParseTree
  ( Program
  , ModuleIntro
  , Def (..)
  , Decl (..)
  , ValCase (..)
  , VarList
  , VarListElem
  , OptWhereClause
  , Expr (..)
  , ExprSeqElem
  , ExprListTypedElem
  , CaseCase
  , ParsePattern (..)

  , astToString -- Printing

  -- Getters
  , defIsData
  , defDecl
  , declLoc
  , defLoc
  , exprLoc
  , valCaseLoc
  , patternLoc
  , defName
  , declName
  , declType
  )
where

import TypeCheck.Term (PreTerm)
import Control.Monad.Trans.State
import Control.Monad.Writer
import Loc (Loc)

-- import M => path/to/mod
-- of i1 (= False)
-- of i2 (= False)
-- export
-- of e1 (= True)
-- of e2 (= True)
--
type ModuleIntro =
  (Maybe (Loc, String), (Loc, String), [(Loc, String, Bool)])

type Program = ([ModuleIntro], [Def])

data Def = DefVal Bool Decl [ValCase] -- Bool = True if pure
         | DefExtern Decl
         | DefData Bool Decl [Decl]   -- Bool = True if pure
         deriving Show

data Decl = Decl (Loc, String) VarList Expr deriving Show

  -- 'let'
  --    [ X ':=' ParsePattern, Y ':=' ParsePattern, ...]
  --    ( ParsePattern, ParsePattern, .. ) '=>' Expr 'where' ... 'end'
data ValCase =
  ValCase Loc [((Loc, String), ParsePattern)]
    (Maybe [ParsePattern]) (Maybe (Expr, OptWhereClause))
  deriving Show

-- x : X, y : Y, z : Z
type VarList = [VarListElem]

type VarListElem = ((Loc, String), Maybe Expr)

type OptWhereClause = Maybe [Def]

data Expr =
    ExprFun (Maybe PreTerm) Loc VarList Expr
      -- ExprArrow Bool = True if it is an IO arrow.
  | ExprArrow Loc Bool [ExprListTypedElem] Expr
  | ExprLazyArrow Loc Bool Expr
  | ExprDelayArrow Loc Bool Expr
  | ExprApp (Maybe PreTerm) Expr [Expr]
  | ExprImplicitApp (Maybe PreTerm) Expr [((Loc, String), Expr)]
  | ExprLazyApp (Maybe PreTerm) Expr
  | ExprVar (Maybe PreTerm) (Loc, String)
  | ExprSeq (Maybe PreTerm) ExprSeqElem Expr
  | ExprCase (Maybe PreTerm) Loc Expr [CaseCase]
  | ExprUnitElem Loc
  | ExprUnitTy Loc
  | ExprTy Loc
  deriving Show

type ExprSeqElem = Either Expr (ParsePattern, Expr)

type ExprListTypedElem = Either Expr ((Loc, String), Expr)

type CaseCase = Either ParsePattern (ParsePattern, Expr)

data ParsePattern = ParsePatternApp ParsePattern [ParsePattern]
                  | ParsePatternLazyApp ParsePattern
                  | ParsePatternImplicitApp ParsePattern
                                            [((Loc, String), ParsePattern)]
                  | ParsePatternVar (Loc, String)
                  | ParsePatternEmpty Loc
                  | ParsePatternUnit Loc
                  deriving Show

type ToString a = StateT (Bool, Int) (Writer String) a

------------------------ Getters ---------------------------------

valCaseLoc :: ValCase -> Loc
valCaseLoc (ValCase lo _ _ _) = lo

declLoc :: Decl -> Loc
declLoc (Decl (lo, _) _ _) = lo

defLoc :: Def -> Loc
defLoc (DefExtern d) = declLoc d
defLoc (DefVal _ d _) = declLoc d
defLoc (DefData _ d _) = declLoc d

patternLoc :: ParsePattern -> Loc
patternLoc (ParsePatternApp p _) = patternLoc p
patternLoc (ParsePatternLazyApp p) = patternLoc p
patternLoc (ParsePatternImplicitApp p _) = patternLoc p
patternLoc (ParsePatternVar (lo, _)) = lo
patternLoc (ParsePatternUnit lo) = lo
patternLoc (ParsePatternEmpty lo) = lo

exprSeqElemLoc :: ExprSeqElem -> Loc
exprSeqElemLoc (Left e) = exprLoc e
exprSeqElemLoc (Right (p, _)) = patternLoc p

exprLoc :: Expr -> Loc
exprLoc (ExprUnitElem lo) = lo
exprLoc (ExprUnitTy lo) = lo
exprLoc (ExprFun _ lo _ _) = lo
exprLoc (ExprArrow lo _ _ _) = lo
exprLoc (ExprLazyArrow lo _ _) = lo
exprLoc (ExprDelayArrow lo _ _) = lo
exprLoc (ExprApp _ e _) = exprLoc e
exprLoc (ExprImplicitApp _ e _) = exprLoc e
exprLoc (ExprLazyApp _ e) = exprLoc e
exprLoc (ExprVar _ (lo, _)) = lo
exprLoc (ExprCase _ lo _ _) = lo
exprLoc (ExprTy lo) = lo
exprLoc (ExprSeq _ e _) = exprSeqElemLoc e

declName :: Decl -> String
declName (Decl (_, s) _ _) = s

defName :: Def -> String
defName (DefExtern d) = declName d
defName (DefVal _ d _) = declName d
defName (DefData _ d _) = declName d

defDecl :: Def -> Decl
defDecl (DefExtern d) = d
defDecl (DefVal _ d _) = d
defDecl (DefData _ d _) = d

defIsData :: Def -> Bool
defIsData (DefData _ _ _) = True
defIsData _ = False

declType :: Decl -> Expr
declType (Decl _ _ t) = t

---------------------- Pretty printing ----------------------------

incIndent :: ToString ()
incIndent = modify (\(b, n) -> (b, n+2))

decIndent :: ToString ()
decIndent = modify (\(b, n) -> (b, n-2))

setMissingIndent :: Bool -> ToString ()
setMissingIndent b = modify (\(_, n) -> (b, n))

getMissingIndent :: ToString Bool
getMissingIndent = fmap fst get

writeIndent :: ToString ()
writeIndent = get >>= doWrite . snd
  where
    doWrite :: Int -> ToString ()
    doWrite 0 = return ()
    doWrite i = tell " " >> doWrite (i - 1)

writeStr :: String -> ToString ()
writeStr s = do
  b <- getMissingIndent
  when b writeIndent
  setMissingIndent False
  tell s

newLine :: ToString ()
newLine = tell "\n" >> setMissingIndent True

writePatternNamedArgList :: [((Loc, String), ParsePattern)] -> ToString ()
writePatternNamedArgList [] = return ()
writePatternNamedArgList ps = doWritePatternNamedArgList ps

doWritePatternNamedArgList :: [((Loc, String), ParsePattern)] -> ToString ()
doWritePatternNamedArgList ps = doWriteArgs ps
  where
    doWriteArgs [] = return ()
    doWriteArgs [((_, s), x)] =
      writeStr "[" >> writeStr s >> writeStr " := " >> writePattern x >> writeStr "]"
    doWriteArgs (((_, s), x) : xs) =
      writeStr "[" >> writeStr s >> writeStr " := " >> writePattern x >> writeStr "] "
      >> doWriteArgs xs

writePatternMaybeArgs :: Maybe [ParsePattern] -> ToString ()
writePatternMaybeArgs Nothing = return ()
writePatternMaybeArgs (Just ps) = writePatternArgs ps

writePatternArgs :: [ParsePattern] -> ToString ()
writePatternArgs ps = doWriteArgs ps
  where
    doWriteArgs [] = return ()
    doWriteArgs [x] = writeStr "(" >> writePattern x >> writeStr ")"
    doWriteArgs (x : xs) =
      writeStr "(" >> writePattern x >> writeStr ") " >> doWriteArgs xs

writePatternImplicitArgs :: [((Loc, String), ParsePattern)] -> ToString ()
writePatternImplicitArgs ps = doWriteArgs ps >> writeStr "]"
  where
    doWriteArgs [] = return ()
    doWriteArgs [((_, v), x)] =
      writeStr "[" >> writeStr v >> writeStr " := " >> writePattern x >> writeStr "]"
    doWriteArgs (x : xs) =
      writeStr "[" >> doWriteArgs [x] >> writeStr "] " >> doWriteArgs xs

writePattern :: ParsePattern -> ToString ()
writePattern (ParsePatternApp p ps) = writePattern p >> writePatternArgs ps
writePattern (ParsePatternLazyApp p) = writePattern p >> writeStr " []"
writePattern (ParsePatternImplicitApp p ps) =
  writePattern p >> writePatternImplicitArgs ps
writePattern (ParsePatternUnit _) = writeStr "()"
writePattern (ParsePatternEmpty _) = writeStr "{}"
writePattern (ParsePatternVar (_, v)) = writeStr v

writeExprListTyped :: String -> [ExprListTypedElem] -> ToString ()
writeExprListTyped sep es = doWrite es
  where
    doWrite :: [ExprListTypedElem] -> ToString ()
    doWrite [] = return ()
    doWrite [Left e] = writeExpr e
    doWrite [Right ((_, v), t)] = do
      writeStr "("
      writeStr v
      writeStr " : "
      writeExpr t
      writeStr ")"
    doWrite (Left x : xs) = writeExpr x >> writeStr ", " >> doWrite xs
    doWrite (Right ((_, v), t) : xs) = do
      writeStr "("
      writeStr v
      writeStr " : "
      writeExpr t
      writeStr ") "
      writeStr sep
      when (sep /= "") (writeStr " ")
      doWrite xs

writeExpr :: Expr -> ToString ()
writeExpr (ExprTy _) = writeStr "Ty"
writeExpr (ExprUnitElem _) = writeStr "()"
writeExpr (ExprUnitTy _) = writeStr "{}"
writeExpr (ExprFun _ _ vs e) = do
  writeVarList vs
  writeStr " => "
  writeExpr e
writeExpr (ExprArrow _ b es e) = do
  writeExprListTyped "&" es
  if b
    then writeStr " ->> "
    else writeStr " -> "
  writeExpr e
writeExpr (ExprLazyArrow _ b e) = do
  if b
    then writeStr "[] ->> "
    else writeStr "[] -> "
  writeExpr e
writeExpr (ExprDelayArrow _ b e) = do
  if b
    then writeStr "() ->> "
    else writeStr "() -> "
  writeExpr e
writeExpr (ExprLazyApp _ e) = do
  writeExpr e
  writeStr "[]"
writeExpr (ExprApp _ e as) =
  let hasLowerPrec (ExprFun _ _ _ _) = True
      hasLowerPrec (ExprArrow _ _ _ _) = True
      hasLowerPrec (ExprLazyArrow _ _ _) = True
      hasLowerPrec (ExprDelayArrow _ _ _) = True
      hasLowerPrec _ = False
      
      writeExprs [] = return ()
      writeExprs [x] = writeExpr x
      writeExprs (x:xs) = writeExpr x >> writeStr ", " >> writeExprs xs
  in do {
      if hasLowerPrec e
        then do
          writeStr "("
          writeExpr e
          writeStr ")"
        else
          writeExpr e;
     writeStr "(";
     writeExprs as;
     writeStr ")" }
writeExpr (ExprImplicitApp _ e as) =
  let hasLowerPrec (ExprFun _ _ _ _) = True
      hasLowerPrec (ExprArrow _ _ _ _) = True
      hasLowerPrec (ExprLazyArrow _ _ _) = True
      hasLowerPrec (ExprDelayArrow _ _ _) = True
      hasLowerPrec _ = False
      
      writeExprs [] = return ()
      writeExprs [((_, n), x)] = writeStr n >> writeStr " = " >> writeExpr x
      writeExprs (x:xs) = writeExprs [x] >> writeStr ", " >> writeExprs xs
  in do {
      if hasLowerPrec e
        then do
          writeStr "("
          writeExpr e
          writeStr ")"
        else
          writeExpr e;
     writeStr "[";
     writeExprs as;
     writeStr "]" }
writeExpr (ExprVar _ (_, v)) = writeStr v
writeExpr (ExprSeq _ e1 e2) = do
  writeStr "("
  incIndent
  writeExprSeqElem e1
  writeExprSeqTail e2
  decIndent
  writeStr ")"
  where
    writeExprSeqElem (Left e) = do
      writeExpr e
      writeStr ";"
      newLine
    writeExprSeqElem (Right (p, e)) = do
      writePattern p
      writeStr " = "
      writeExpr e
      writeStr ";"
      newLine
    writeExprSeqTail (ExprSeq _ e1' e2') =
      writeExprSeqElem e1' >> writeExprSeqTail e2'
    writeExprSeqTail e = writeExpr e
writeExpr (ExprCase _ _ e ofs) = do
  writeStr "case "
  writeExpr e
  newLine
  let writeOfs :: [CaseCase] -> ToString ()
      writeOfs [] = return ()
      writeOfs (Right (p, x) : os) = do
        writeStr "of "
        writePattern p
        writeStr " => "
        incIndent
        newLine
        writeExpr x
        decIndent
        newLine
        writeOfs os
      writeOfs (Left p : os) = do
        writeStr "of "
        writePattern p
        writeStr " {}"
        newLine
        writeOfs os
  writeOfs ofs
  writeStr "end"

writeVarListElem :: VarListElem -> ToString ()
writeVarListElem ((_, s), Nothing) = writeStr s
writeVarListElem ((_, s), Just t) = do
  writeStr "("
  writeStr s
  writeStr " : "
  writeExpr t
  writeStr ")"

writeVarList :: VarList -> ToString ()
writeVarList [] = return ()
writeVarList [x] = writeVarListElem x
writeVarList (x : xs) = do
  writeVarListElem x
  writeStr " "
  writeVarList xs

writeOptWhere :: Maybe [Def] -> ToString ()
writeOptWhere Nothing = return ()
writeOptWhere (Just ds) = do
  writeStr "where"
  incIndent
  newLine
  writeDefs ds
  decIndent
  writeStr "end"
  newLine

writeNameDecl :: Decl -> ToString ()
writeNameDecl (Decl (_, v) imps t) = do
  writeStr v
  writeNamedVarList imps
  writeStr " : "
  writeExpr t

writeNamedVarList :: VarList -> ToString ()
writeNamedVarList [] = return ()
writeNamedVarList vs =
  writeStr "[" >> doWriteNamedVarList vs >> writeStr "]"

doWriteNamedVarList :: VarList -> ToString ()
doWriteNamedVarList [] = return ()
doWriteNamedVarList [((_, na), Just t)] =
  writeStr na >> writeStr " = " >> writeExpr t >> writeStr ", "
doWriteNamedVarList [((_, na), Nothing)] =
  writeStr na
doWriteNamedVarList (((_, na), Just t) : vs) =
  writeStr na >> writeStr " = " >> writeExpr t >> writeStr ", "
  >> doWriteNamedVarList vs
doWriteNamedVarList (((_, na), Nothing) : vs) =
  writeStr na >> writeStr ", " >> doWriteNamedVarList vs

writeValCase :: ValCase -> ToString ()
writeValCase (ValCase _ ps0 ps (Just (e, w))) = do
  writeStr "let "
  writePatternNamedArgList ps0
  writePatternMaybeArgs ps
  writeStr " =>"
  incIndent
  newLine
  writeExpr e
  decIndent
  newLine
  writeOptWhere w
writeValCase (ValCase _ ps0 ps Nothing) = do
  writeStr "let "
  writePatternNamedArgList ps0
  writePatternMaybeArgs ps
  newLine

writeDef :: Def -> ToString ()
writeDef (DefExtern d) = do
  writeStr "extern "
  writeNameDecl d
  newLine
writeDef (DefVal isPure d ls) = do
  if isPure
    then writeStr "val "
    else writeStr "val.. "
  writeNameDecl d
  newLine
  mapM_ writeValCase ls
writeDef (DefData isPure (Decl (_, v) imps t) vs) = do
  if isPure
    then writeStr ("data " ++ v)
    else writeStr ("data.. " ++ v)
  writeNamedVarList imps
  writeStr " : "
  writeExpr t
  let ctorDecls :: [Decl] -> ToString ()
      ctorDecls [] = return ()
      ctorDecls (x:xs) =
        newLine >> writeStr "let " >> writeNameDecl x >> ctorDecls xs
  ctorDecls vs
  newLine

writeDefs :: [Def] -> ToString ()
writeDefs [] = return ()
writeDefs [x] = writeDef x
writeDefs (x : xs) = do
  writeDef x
  newLine
  writeDefs xs

writeModuleIntros :: [ModuleIntro] -> ToString ()
writeModuleIntros [] = return ()
writeModuleIntros [x] = do
  writeModuleIntro x
  newLine
  newLine
writeModuleIntros (x : xs) = do
  writeModuleIntro x
  newLine
  writeModuleIntros xs

writeModuleIntro :: ModuleIntro -> ToString ()
writeModuleIntro (Nothing, path, ies) = do
  writeStr "import "
  writeStr (snd path)
  writeModuleImportExportList ies
writeModuleIntro (Just m, path, ies) = do
  writeStr "import "
  writeStr (snd m)
  writeStr " => "
  writeStr (snd path)
  writeModuleImportExportList ies

writeModuleImportExportList :: [(Loc, String, Bool)] -> ToString ()
writeModuleImportExportList [] = return ()
writeModuleImportExportList [(_, s, False)] = do
  writeStr "of " >> writeStr s
writeModuleImportExportList ((_, s, False) : ss) = do
  writeStr "of " >> writeStr s
  newLine
  writeModuleImportExportList ss
writeModuleImportExportList ss@((_, _, True) : _) = do
  writeStr "export"
  newLine
  writeModuleExportList ss

writeModuleExportList :: [(Loc, String, Bool)] -> ToString ()
writeModuleExportList [] = return ()
writeModuleExportList [(_, s, _)] =
  writeStr "of " >> writeStr s
writeModuleExportList ((_, s, _) : ss) = do
  writeStr "of " >> writeStr s
  newLine
  writeModuleExportList ss

writeProgram :: Program -> ToString ()
writeProgram (is, ds) = do
  writeModuleIntros is
  writeDefs ds

runToString :: Program -> String
runToString p = snd (runWriter (runStateT (writeProgram p) (True, 0)))

astToString :: Program -> String
astToString = runToString
