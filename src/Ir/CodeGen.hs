{-# LANGUAGE BangPatterns #-}

module Ir.CodeGen where

import qualified Str as Str
import qualified Ir.RefCountIr as R
import qualified System.IO as SysIO
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad (when)
import qualified Data.List as List
import Data.Maybe (isNothing, isJust)

import Control.Exception (assert)
--import Debug.Trace (trace)

type GenCode = StateT (Int, SysIO.Handle, R.Program) IO

genCode :: R.Program -> FilePath -> IO ()
genCode p s =
  SysIO.withFile s SysIO.WriteMode (runGenCode p)

runGenCode :: R.Program -> SysIO.Handle -> IO ()
runGenCode p fh = evalStateT (writeProgram p) (0, fh, p)

writeStr :: String -> GenCode ()
writeStr s = do
  fh <- getFile
  liftIO (SysIO.hPutStr fh s)

incIndent :: GenCode ()
incIndent = modify (\(n, x, y) -> (n+2, x, y))

decIndent :: GenCode ()
decIndent = modify (\(n, x, y) -> (n-2, x, y))

getFile :: GenCode SysIO.Handle
getFile = fmap (\(_, x, _) -> x) get

constArity :: R.Const -> GenCode Int
constArity c = do
  p <- fmap (\(_, _, y) -> y) get
  let e = R.lookupProgram c p
  case e of
    R.Extern vs -> return (length vs)
    R.Fun vs _ -> return (length vs)
    R.Lazy _ vs _ -> return (length vs)

writeIndent :: GenCode ()
writeIndent = get >>= doWrite . (\(a, _, _) -> a)
  where
    doWrite :: Int -> GenCode ()
    doWrite 0 = return ()
    doWrite i = writeStr " " >> doWrite (i - 1)

newLine :: GenCode ()
newLine = writeStr "\n" >> writeIndent

writeStructYuRef :: GenCode ()
writeStructYuRef = writeStr "yur_Ref"

writeStructYuRefPtr :: GenCode ()
writeStructYuRefPtr = writeStructYuRef >> writeStr " *"

writeVarDecl :: R.Var -> GenCode ()
writeVarDecl v = do
  writeStructYuRefPtr
  writeStr (varName v)

writeVarDecls :: [R.Var] -> GenCode ()
writeVarDecls [] = return ()
writeVarDecls [v] = writeVarDecl v
writeVarDecls (v : vs) = writeVarDecls vs >> writeStr ", " >> writeVarDecl v

writeParenVars :: [R.Var] -> GenCode ()
writeParenVars vs = writeStr "(" >> writeVarDecls vs >> writeStr ")"

writeVar :: R.Var -> GenCode ()
writeVar v = writeStr (varName v)

writeRef :: R.Ref -> GenCode ()
writeRef (R.VarRef v) = writeVar v
writeRef (R.ConstRef r) = writeStr (funRef r)

writeRefs :: [R.Ref] -> GenCode ()
writeRefs [] = return ()
writeRefs [v] = writeRef v
writeRefs (v : vs) = writeRefs vs >> writeStr ", " >> writeRef v

writeParenRefs :: [R.Ref] -> GenCode ()
writeParenRefs vs = writeStr "(" >> writeRefs vs >> writeStr ")"

writeDecRefsLn :: [R.Ref] -> GenCode ()
writeDecRefsLn [] = return ()
writeDecRefsLn (v : vs) = do
  writeDecRefLn v
  writeDecRefsLn vs

writeProgram :: R.Program -> GenCode ()
writeProgram p = do
  let cs = IntMap.elems p
  writePrelude
  writeDecls cs
  newLine
  newLine
  evalStateT (writeTrivialCtors cs) IntSet.empty
  evalStateT (writePapClosures cs) Set.empty
  writeConsts cs
  where
    writeDecls :: [(R.Const, R.ConstExpr)] -> GenCode ()
    writeDecls [] = return ()
    writeDecls [(c, e)] = writeDecl c e >> return ()
    writeDecls ((c, e) : cs) = do
      writeDecl c e
      newLine
      newLine
      writeDecls cs

    writeTrivialCtors :: [(R.Const, R.ConstExpr)] -> StateT IntSet GenCode ()
    writeTrivialCtors [] = return ()
    writeTrivialCtors [(_,e)] = writeTrivialCtorsConstExpr e
    writeTrivialCtors ((_,e) : cs) = do
      writeTrivialCtorsConstExpr e
      writeTrivialCtors cs

    writePapClosures ::
      [(R.Const, R.ConstExpr)] ->
      StateT (Set (R.Const, Int)) GenCode ()
    writePapClosures [] = return ()
    writePapClosures [(_,e)] = writePapClosuresConstExpr e
    writePapClosures ((_,e) : cs) = do
      writePapClosuresConstExpr e
      writePapClosures cs

    writeConsts :: [(R.Const, R.ConstExpr)] -> GenCode ()
    writeConsts [] = return ()
    writeConsts [(c, e)] = writeConst c e
    writeConsts ((c, e) : cs) = do
      writeConst c e
      newLine
      newLine
      writeConsts cs

writePrelude :: GenCode ()
writePrelude = writeStr "#include \"yu.h\"" >> newLine >> newLine

writeTrivialCtorsConstExpr :: R.ConstExpr -> StateT IntSet GenCode ()
writeTrivialCtorsConstExpr (R.Extern _) = return ()
writeTrivialCtorsConstExpr (R.Fun _ e) =
  writeTrivialCtorsFunExpr e
writeTrivialCtorsConstExpr (R.Lazy _ _ e) =
  writeTrivialCtorsFunExpr e

writeTrivialCtorsFunExpr :: R.FunExpr -> StateT IntSet GenCode ()
writeTrivialCtorsFunExpr (R.Case _ cs) =
  mapM_ (\(_, c) -> writeTrivialCtorsFunExpr c) cs
writeTrivialCtorsFunExpr (R.Pforce _ _ _ e2) =
  writeTrivialCtorsFunExpr e2
writeTrivialCtorsFunExpr (R.Let _ e1 e2) = do
  writeTrivialCtorsLetExpr e1
  writeTrivialCtorsFunExpr e2
writeTrivialCtorsFunExpr (R.Ret _) = return ()
writeTrivialCtorsFunExpr (R.Inc _ e) =
  writeTrivialCtorsFunExpr e
writeTrivialCtorsFunExpr (R.Dec _ e) =
  writeTrivialCtorsFunExpr e
writeTrivialCtorsFunExpr (R.Unuse _ e) =
  writeTrivialCtorsFunExpr e

writeTrivialCtorsLetExpr :: R.LetExpr -> StateT IntSet GenCode ()
writeTrivialCtorsLetExpr (R.CtorBox c []) = do
  gn <- get
  if IntSet.member (R.prCtor c) gn
  then return ()
  else do
    put (IntSet.insert (R.prCtor c) gn)
    lift (writeTrivialCtor c)
writeTrivialCtorsLetExpr _ = return ()

writeTrivialCtor :: R.Ctor -> GenCode ()
writeTrivialCtor c = do
  when (R.prCtor c /= 0) $ do
    writeStructYuRef
    writeStr (" " ++ trivialCtorName c ++ " = {") >> newLine
    writeStr ("  .count = 0,") >> newLine
    writeStr ("  .tag = " ++ show (R.prCtor c) ++ ",") >> newLine
    writeStr ("  .vmt_index = yur_Static_vmt,") >> newLine
    writeStr ("  .nfields = 0") >> newLine
    writeStr ("};") >> newLine
    newLine

writePapClosuresConstExpr ::
  R.ConstExpr -> StateT (Set (R.Const, Int)) GenCode ()
writePapClosuresConstExpr (R.Extern _) = return ()
writePapClosuresConstExpr (R.Fun _ e) = writePapClosuresFunExpr e
writePapClosuresConstExpr (R.Lazy _ _ e) =
  writePapClosuresFunExpr e

writePapClosuresFunExpr ::
  R.FunExpr -> StateT (Set (R.Const, Int)) GenCode ()
writePapClosuresFunExpr (R.Case _ cs) =
  mapM_ (\(_, c) -> writePapClosuresFunExpr c) cs
writePapClosuresFunExpr (R.Pforce _ _ _ e2) =
  writePapClosuresFunExpr e2
writePapClosuresFunExpr (R.Let _ e1 e2) = do
  writePapClosuresLetExpr e1
  writePapClosuresFunExpr e2
writePapClosuresFunExpr (R.Ret _) = return ()
writePapClosuresFunExpr (R.Inc _ e) = writePapClosuresFunExpr e
writePapClosuresFunExpr (R.Dec _ e) = writePapClosuresFunExpr e
writePapClosuresFunExpr (R.Unuse _ e) = writePapClosuresFunExpr e

writePapClosuresLetExpr ::
  R.LetExpr -> StateT (Set (R.Const, Int)) GenCode ()
writePapClosuresLetExpr (R.Pap c rs) =
  writePapClosure c Nothing rs
writePapClosuresLetExpr (R.MkLazy False (r, rs) c) =
  writePapClosure c (Just r) rs
writePapClosuresLetExpr _ = return ()

writePapClosure ::
  R.Const -> Maybe R.Var -> [R.Ref] -> StateT (Set (R.Const, Int)) GenCode ()
writePapClosure c v rs = do
  let n = length rs
  gn <- get
  if Set.member (c, n) gn
  then return ()
  else do
    put (Set.insert (c, n) gn)
    lift (writePapFunClosure c v rs n)
    when (isJust v) (lift (writeLazyFun False c n >> newLine >> newLine))

writePapFunClosure :: R.Const -> Maybe R.Var -> [R.Ref] -> Int -> GenCode ()
writePapFunClosure c v rs n = do
  let recIdx = case v of
                Nothing -> Nothing
                Just v' -> List.elemIndex (R.VarRef v') rs
  a <- constArity c
  let nparams = a - n
  writeStructYuRefPtr
  writeStr (papClosureName c n)
  writeStr "("
  writeParams nparams 
  writeStr ") {"
  incIndent >> newLine
  writeTempsAndIncs recIdx 0
  if isNothing v
  then writeStr "yur_unref(self);" >> newLine
  else return ()
  writeStr "return "
  writeStr (funName c)
  writeStr "("
  writeArgs nparams
  when (n > 0 && nparams > 0) (writeStr ", ")
  writeAppliedArgs recIdx n
  writeStr ");"
  decIndent >> newLine
  writeStr "}" >> newLine
  newLine
  where
    paramName :: Int -> String
    paramName i = 'a' : show i

    convertIdx :: Maybe Int -> Int -> Int    
    convertIdx recIdx i =
      let !() = assert (recIdx /= Just i) ()
      in case recIdx of
          Nothing -> i
          Just ri -> if ri < i then i - 1 else i

    tempName :: Maybe Int -> Int -> String
    tempName recIdx i =
      if recIdx == Just i
        then "self"
        else 'b' : show (convertIdx recIdx i)

    writeParams :: Int -> GenCode ()
    writeParams 0 = writeStructYuRefPtr >> writeStr "self"
    writeParams i = do
      writeStructYuRefPtr >> writeStr (paramName i)
      writeStr ", "
      writeParams (i-1)

    writeArgs :: Int -> GenCode ()
    writeArgs 0 = return ()
    writeArgs 1 = writeStr (paramName 1)
    writeArgs i = do
      writeStr (paramName i)
      writeStr ", "
      writeArgs (i-1)

    writeFieldEntry :: Maybe Int -> Int -> GenCode ()
    writeFieldEntry recIdx i =
      if isJust v
      then writeStr ("self->fields[" ++ show (convertIdx recIdx i + 1) ++ "]")
      else writeStr ("self->fields[" ++ show i ++ "]")

    writeTempsAndIncs :: Maybe Int -> Int -> GenCode ()
    writeTempsAndIncs recIdx i =
      if i >= n
      then return ()
      else do
        if recIdx == Just i
           then writeStr "yur_inc(self);" >> newLine
           else do
            writeStructYuRefPtr
            let b = tempName recIdx i
            writeStr (b ++ " = ")
            writeFieldEntry recIdx i
            writeStr ";" >> newLine
            writeStr ("yur_inc(" ++ b ++ ");") >> newLine
        writeTempsAndIncs recIdx (i+1)

    writeAppliedArgs :: Maybe Int -> Int -> GenCode ()
    writeAppliedArgs _ 0 = return ()
    writeAppliedArgs recIdx 1 = writeStr (tempName recIdx 0)
    writeAppliedArgs recIdx i = do
      writeStr (tempName recIdx (i-1) ++ ", ")
      writeAppliedArgs recIdx (i-1)

papClosureName :: R.Const -> Int -> String
papClosureName c i =
  Str.genMachineName (Str.papClosurePrefix ++ R.nameConst c ++ show i)

trivialCtorName :: R.Ctor -> String
trivialCtorName c =
  if R.prCtor c /= 0
  then Str.genMachineName (Str.trivialCtorPrefix ++ show (R.prCtor c))
  else "yur_unit"

trivialCtorRef :: R.Ctor -> String
trivialCtorRef c = '&' : trivialCtorName c

varName :: R.Var -> String
varName v = 'x' : show v

funName :: R.Const -> String
funName c = Str.genMachineName (R.nameConst c)

funImpl :: R.Const -> String
funImpl c = Str.genMachineName (Str.implementationPrefix ++ R.nameConst c)

lazyFun :: R.Const -> Int -> String
lazyFun c n =
  Str.genMachineName (
    Str.implementationPrefix
    ++ Str.papClosurePrefix
    ++ R.nameConst c ++ show n)

lazyRef :: R.Const -> Int -> String
lazyRef c n = '&' : lazyFun c n

funRef :: R.Const -> String
funRef c = '&' : funImpl c

writeFunDecl :: R.Const -> [R.Var] -> GenCode ()
writeFunDecl c vs = do
  writeStructYuRefPtr
  writeStr (funName c)
  writeParenVars vs

writeFunImpl :: R.Const -> GenCode ()
writeFunImpl c = do
  writeStructYuRef >> writeStr " " >> writeStr (funImpl c)
  writeStr (" = {") >> newLine
  writeStr ("  .count = 0,") >> newLine
  writeStr ("  .tag = (size_t) &" ++ papClosureName c 0 ++ ",") >> newLine
  writeStr ("  .vmt_index = yur_Static_vmt,") >> newLine
  writeStr ("  .nfields = 0") >> newLine
  writeStr ("};");

writeLazyFun :: Bool -> R.Const -> Int -> GenCode ()
writeLazyFun _isStatic c n = do
  writeStructYuRef >> writeStr " " >> writeStr (lazyFun c n)
  writeStr (" = {") >> newLine
  writeStr ("  .count = 0,") >> newLine
  writeStr ("  .tag = (size_t) &" ++ papClosureName c n ++ ",") >> newLine
  writeStr ("  .vmt_index = yur_Static_vmt,") >> newLine
  writeStr ("  .nfields = 0") >> newLine
  writeStr ("};")

writeLazyImpl :: R.Const -> GenCode ()
writeLazyImpl c = do
  writeStructYuRef >> writeStr " " >> writeStr (funImpl c)
  writeStr (" = {") >> newLine
  writeStr ("  .count = 0,") >> newLine
  writeStr ("  .tag = 0,") >> newLine
  writeStr ("  .vmt_index = yur_Static_vmt,") >> newLine
  writeStr ("  .nfields = 1,") >> newLine
  writeStr ("  .fields[0] = (yur_Ref *) &" ++ lazyFun c 0) >> newLine
  writeStr ("};")

writeDecl :: R.Const -> R.ConstExpr -> GenCode ()
writeDecl c (R.Extern vs) = do
  writeFunDecl c vs
  writeStr ";"
  newLine
  newLine
  writePapFunClosure c Nothing [] 0
  writeFunImpl c
writeDecl c (R.Fun vs _) = do
  writeFunDecl c vs
  writeStr ";"
  newLine
  newLine
  writePapFunClosure c Nothing [] 0
  writeFunImpl c
writeDecl c (R.Lazy iss vs _) = do
  writeFunDecl c vs >> writeStr ";"
  when iss $ do
    newLine
    newLine
    writePapFunClosure c Nothing [] 0
    writeLazyFun True c 0
    newLine
    newLine
    writeLazyImpl c

writeConst :: R.Const -> R.ConstExpr -> GenCode ()
writeConst _ (R.Extern _) = return ()
writeConst c (R.Fun vs e) = writeFun c vs e
writeConst c (R.Lazy _ vs e) = writeFun c vs e

writeFun :: R.Const -> [R.Var] -> R.FunExpr -> GenCode ()
writeFun c vs e = do
  writeFunDecl c vs >> writeStr " {"
  incIndent >> newLine
  writeFunExpr e
  decIndent
  newLine
  writeStr "}"

writeRefTag :: R.Ref -> GenCode ()
writeRefTag (R.VarRef v) = writeVar v >> writeStr "->tag"
writeRefTag (R.ConstRef r) = writeStr (funImpl r) >> writeStr ".tag"

writeFunExprCases :: [(R.CtorId, R.FunExpr)] -> GenCode ()
writeFunExprCases [] = error "missing case"
writeFunExprCases [(_, e)] = do
  newLine
  writeStr ("default: {")
  incIndent >> newLine
  writeFunExpr e
  decIndent >> newLine
  writeStr "}"
writeFunExprCases ((i, e) : cs) = do
  newLine
  writeStr ("case " ++ show i ++ ": {")
  incIndent >> newLine
  writeFunExpr e
  decIndent >> newLine
  writeStr "}"
  writeFunExprCases cs

writeFunExpr :: R.FunExpr -> GenCode ()
writeFunExpr (R.Case r cs) = do
  writeStr "switch (" >> writeRefTag r >> writeStr ") {"
  writeFunExprCases cs
  newLine
  writeStr "}"
writeFunExpr (R.Pforce _ v rs e2) = do
  let rs' = filter (/= R.VarRef v) rs
  writeVar v >> writeStr "->nfields = "
    >> writeStr (show (length rs' + 1)) >> writeStr ";" >> newLine
  setVarFields v 1 rs' >> newLine
  writeFunExpr e2
writeFunExpr (R.Let v e1 e2) = do
  case e1 of
    R.Reset _ -> return ()
    _ -> writeVarDecl v >> writeStr ";" >> newLine
  writeLetExpr v e1 >> newLine
  writeFunExpr e2
writeFunExpr (R.Ret r) =
  writeStr "return " >> writeRef r >> writeStr ";"
writeFunExpr (R.Inc v e) = do
  writeIncRefLn v
  writeFunExpr e
writeFunExpr (R.Dec v e) = do
  writeDecRefLn v
  writeFunExpr e
writeFunExpr (R.Unuse v e) = do
  writeStr "yur_dealloc(" >> writeRef v >> writeStr ");" >> newLine
  writeFunExpr e

writeIncRefLn :: R.Ref -> GenCode ()
writeIncRefLn (R.VarRef v) =
  writeStr "yur_inc(" >> writeVar v >> writeStr ");" >> newLine
writeIncRefLn (R.ConstRef _) = return ()

writeDecRefLn :: R.Ref -> GenCode ()
writeDecRefLn (R.VarRef v) =
  writeStr "yur_unref(" >> writeVar v >> writeStr ");" >> newLine
writeDecRefLn (R.ConstRef _) = return ()

writeFunType :: Int -> GenCode ()
writeFunType args = do
  writeStructYuRefPtr >> writeStr "(*)("
  doWrite args
  writeStr ")"
  where
    doWrite :: Int -> GenCode ()
    doWrite 0 = return ()
    doWrite 1 = writeStructYuRefPtr
    doWrite i = writeStructYuRefPtr >> writeStr ", " >> doWrite (i-1)

writeFunCast :: Int -> GenCode ()
writeFunCast args =
  writeStr "(" >> writeFunType args >> writeStr ")"

setFields :: String -> Int -> [R.Ref] -> GenCode ()
setFields _ _ [] = return ()
setFields prefix i (s:ss) = do
  writeStr prefix
  writeStr ("fields[" ++ show i ++ "] = ")
  writeRef s
  writeStr ";"
  when (not (null ss)) newLine
  setFields prefix (i+1) ss

setVarFields :: R.Var -> Int -> [R.Ref] -> GenCode ()
setVarFields v = setFields (varName v ++ "->")

writeLetExpr :: R.Var -> R.LetExpr -> GenCode ()
writeLetExpr letVar (R.MkLazy True _ _) = do
  writeVar letVar >> writeStr (" = yur_alloc(1);") >> newLine
  writeStr "yur_init(" >> writeVar letVar >> writeStr ", 0, 0);"
  newLine
  writeVar letVar >> writeStr "->fields[0] = 0;"
writeLetExpr letVar (R.MkLazy False _ c) = do
  a <- constArity c
  writeVar letVar
  writeStr (" = yur_alloc(" ++ show (a+1) ++ ");") >> newLine
  writeStr "yur_init(" >> writeVar letVar >> writeStr ", 1, 0);" >> newLine
  writeVar letVar >> writeStr "->fields[0] = ("
  writeStructYuRefPtr >> writeStr ") "
  writeStr (lazyRef c a) >> writeStr ";"
writeLetExpr letVar (R.Ap _ (R.ConstRef r) rs) = do
  writeStr (varName letVar) >> writeStr " = ("
  writeFunCast (length rs) >> writeStr (funName r) >> writeStr ")"
  writeParenRefs rs
  writeStr ";"
writeLetExpr letVar (R.Ap _ (R.VarRef r) rs) = do
  writeStr (varName letVar) >> writeStr " = ("
  writeFunCast (length rs + 1) >> writeStr " " >> writeRefTag (R.VarRef r)
  writeStr ")("
  writeRefs rs
  when (not (null rs)) (writeStr ", ")
  writeStr (varName r)
  writeStr ");"
writeLetExpr letVar (R.Pap c rs) = do
  let n = length rs
  writeStr (varName letVar)
    >> writeStr
          (" = yur_build("
               ++ show n ++ ", "
               ++ "(size_t) &" ++ papClosureName c n ++ ");")
  newLine
  setVarFields letVar 0 rs
writeLetExpr letVar (R.Force _ (R.ConstRef r) rs) = do
  writeStr (varName letVar) >> writeStr " = "
  writeStr "yur_ALOAD("
    >> writeStr (funImpl r) >> writeStr ".fields[0]);"
  newLine
  writeStr "if (yur_LIKELY(yur_ALOAD(" >> writeStr (funImpl r) >> writeStr ".tag))) {"
  incIndent >> newLine
  writeDecRefsLn (R.forceProjArgs rs)
  decIndent >> newLine
  writeStr "} else {"
  incIndent >> newLine
  writeStructYuRefPtr >> writeStr "expect = "
    >> writeStr (varName letVar) >> writeStr ";" >> newLine
  writeStr (varName letVar) >> writeStr " = " >> writeStr (funName r)
    >> writeParenRefs (R.forceProjArgs rs) >> writeStr ";"
  newLine
  writeStr "yur_memoize(" >> writeStr (funRef r) >> writeStr ", &"
    >> writeStr (funImpl r) >> writeStr ".fields[0], &"
    >> writeStr (varName letVar) >> writeStr ", expect);" >> newLine
  writeStr "yur_ASTORE(" >> writeStr (funImpl r) >> writeStr ".tag, 1);"
  decIndent >> newLine
  writeStr "}" >> newLine
  writeStr "yur_inc(" >> writeVar letVar >> writeStr ");"
writeLetExpr letVar (R.Force _ (R.VarRef r) Nothing) = do
  writeStr (varName letVar) >> writeStr " = "
  writeStr "yur_ALOAD("
    >> writeStr (varName r) >> writeStr "->fields[0]);"
  writeStr "if (!yur_ALOAD(" >> writeVar r >> writeStr "->tag)) {"
  incIndent >> newLine
  writeStructYuRefPtr >> writeStr "expect = "
    >> writeStr (varName letVar) >> writeStr ";" >> newLine
  writeStr (varName letVar) >> writeStr " = ("
  writeFunCast 1 >> writeStr " "
    >> writeStr (varName letVar) >> writeStr "->tag)("
    >> writeStr (varName r) >> writeStr ");" >> newLine
  writeStr "yur_memoize(" >> writeVar r >> writeStr ", &"
    >> writeStr (varName r) >> writeStr "->fields[0], &"
    >> writeStr (varName letVar) >> writeStr ", expect);" >> newLine
  writeStr "yur_ASTORE(" >> writeVar r >> writeStr "->tag, 1);"
  decIndent >> newLine
  writeStr "}" >> newLine
  writeStr "yur_inc(" >> writeVar letVar >> writeStr ");"
writeLetExpr letVar (R.Force _ (R.VarRef r) (Just (c, rs))) = do
  writeStr (varName letVar) >> writeStr " = "
  writeStr "yur_ALOAD("
    >> writeStr (varName r) >> writeStr "->fields[0]);"
  newLine
  writeStr "if (yur_ALOAD(" >> writeVar r >> writeStr "->tag)) {"
  incIndent >> newLine
  writeDecRefsLn rs
  decIndent >> newLine
  writeStr "} else {"
  incIndent >> newLine
  writeStructYuRefPtr >> writeStr "expect = "
    >> writeStr (varName letVar) >> writeStr ";" >> newLine
  writeStr (varName letVar) >> writeStr " = "
  writeStr (funName c)
    >> writeStr "(" >> writeRefs rs >> writeStr ");" >> newLine
  writeStr "yur_ASTORE(" >> writeVar r >> writeStr "->nfields, 1);" >> newLine
  writeStr "yur_memoize(" >> writeVar r >> writeStr ", &"
    >> writeStr (varName r) >> writeStr "->fields[0], &"
    >> writeStr (varName letVar) >> writeStr ", expect);" >> newLine
  writeStr "yur_ASTORE(" >> writeVar r >> writeStr "->tag, 1);"
  decIndent >> newLine
  writeStr "}" >> newLine
  writeStr "yur_inc(" >> writeVar letVar >> writeStr ");"
writeLetExpr letVar (R.CtorBox c []) = do
  writeStr (varName letVar)
  writeStr " = "
  writeStr (trivialCtorRef c)
  writeStr ";"
writeLetExpr letVar (R.CtorBox c rs@(_:_)) = do
  let n = length rs
  writeStr (varName letVar)
  writeStr (" = yur_build(" ++ show n ++ ", " ++ show (R.prCtor c) ++ ");")
  newLine
  setVarFields letVar 0 rs
writeLetExpr letVar (R.Proj i r) = do
  writeStr (varName letVar)
  writeStr " = ("
  writeRef r
  writeStr (")->fields[" ++ show i ++ "];")
writeLetExpr letVar (R.Reset n) = do
  writeVar letVar
  writeStr " = "
  writeStr "yur_reset(" >> writeVar letVar
    >> writeStr ", " >> writeStr (show n) >> writeStr ");"
writeLetExpr letVar (R.Reuse v c rs) = do
  writeStr (varName letVar) >> writeStr " = "
    >> writeVar v >> writeStr ";" >> newLine
  writeStr (varName letVar) >> writeStr "->tag = "
    >> writeStr (show (R.prCtor c)) >> writeStr ";" >> newLine
  setVarFields letVar 0 rs
