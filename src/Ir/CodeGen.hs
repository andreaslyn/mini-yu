{-# LANGUAGE BangPatterns #-}

module Ir.CodeGen
  ( genCode
  ) where

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

type GenCode = StateT (Int, SysIO.Handle, SysIO.Handle, R.Program) IO

genCode :: R.Program -> FilePath -> FilePath -> [FilePath] -> IO ()
genCode prog cfile hfile himports =
  SysIO.withFile cfile SysIO.WriteMode $ \ch ->
  SysIO.withFile hfile SysIO.WriteMode $ \hh ->
  runGenCode prog hfile himports ch hh

runGenCode ::
  R.Program ->
  FilePath ->
  [FilePath] ->
  SysIO.Handle ->
  SysIO.Handle ->
  IO ()
runGenCode p hpath himports ch hh =
  evalStateT (writeProgram p hpath himports) (0, ch, hh, p)

writeHStr :: String -> GenCode ()
writeHStr s = do
  h <- getHHandle
  liftIO (SysIO.hPutStr h s)

writeCStr :: String -> GenCode ()
writeCStr s = do
  h <- getCHandle
  liftIO (SysIO.hPutStr h s)

data FF = HFile | CFile

writeStr :: FF -> String -> GenCode ()
writeStr HFile = writeHStr
writeStr CFile = writeCStr

incIndent :: GenCode ()
incIndent = modify (\(n, x, y, z) -> (n+2, x, y, z))

decIndent :: GenCode ()
decIndent = modify (\(n, x, y, z) -> (n-2, x, y, z))

getCHandle :: GenCode SysIO.Handle
getCHandle = fmap (\(_, x, _, _) -> x) get

getHHandle :: GenCode SysIO.Handle
getHHandle = fmap (\(_, _, x, _) -> x) get

constArity :: R.Const -> GenCode Int
constArity c = do
  p <- fmap (\(_, _, _, z) -> z) get
  let e = R.lookupProgram c p
  case e of
    R.Hidden a -> return a
    R.Extern vs -> return (length vs)
    R.Fun vs _ -> return (length vs)
    R.Lazy _ vs _ -> return (length vs)

writeIndent :: FF -> GenCode ()
writeIndent ff = get >>= doWrite . (\(a, _, _, _) -> a)
  where
    doWrite :: Int -> GenCode ()
    doWrite 0 = return ()
    doWrite i = writeStr ff " " >> doWrite (i - 1)

newLine :: FF -> GenCode ()
newLine ff = writeStr ff "\n" >> writeIndent ff

writeStructYuRef :: FF -> GenCode ()
writeStructYuRef ff = writeStr ff "yur_Ref"

writeStructYuRefPtr :: FF -> GenCode ()
writeStructYuRefPtr ff = writeStructYuRef ff >> writeStr ff " *"

writeVarDecl :: FF -> R.Var -> GenCode ()
writeVarDecl ff v = do
  writeStructYuRefPtr ff
  writeStr ff (varName v)

writeVarDecls :: FF -> [R.Var] -> GenCode ()
writeVarDecls _ [] = return ()
writeVarDecls ff [v] = writeVarDecl ff v
writeVarDecls ff (v : vs) =
  writeVarDecls ff vs >> writeStr ff ", " >> writeVarDecl ff v

writeParenVars :: FF -> [R.Var] -> GenCode ()
writeParenVars ff vs =
  writeStr ff "(" >> writeVarDecls ff vs >> writeStr ff ")"

writeVar :: FF -> R.Var -> GenCode ()
writeVar ff v = writeStr ff (varName v)

writeRef :: FF -> R.Ref -> GenCode ()
writeRef ff (R.VarRef v) = writeVar ff v
writeRef ff (R.ConstRef r) = writeStr ff (funRef r)

writeRefs :: FF -> [R.Ref] -> GenCode ()
writeRefs _ [] = return ()
writeRefs ff [v] = writeRef ff v
writeRefs ff (v : vs) =
  writeRefs ff vs >> writeStr ff ", " >> writeRef ff v

writeParenRefs :: FF -> [R.Ref] -> GenCode ()
writeParenRefs ff vs =
  writeStr ff "(" >> writeRefs ff vs >> writeStr ff ")"

writeDecRefsLn :: [R.Ref] -> GenCode ()
writeDecRefsLn rs = writeDecRefsLnAcc 0 [] rs

writeDecRefsLnAcc :: Int -> [R.Var] -> [R.Ref] -> GenCode ()
writeDecRefsLnAcc 6 acc rs = do
  writeStr CFile "yur_unref_6("
  writeVarArgsList acc
  writeStr CFile ");"
  newLine CFile
  writeDecRefsLnAcc 0 [] rs
writeDecRefsLnAcc _ [] [] = return ()
writeDecRefsLnAcc i acc [] = do
  writeStr CFile ("yur_unref_" ++ show i ++ "(")
  writeVarArgsList acc
  writeStr CFile ");"
  newLine CFile
writeDecRefsLnAcc i acc (R.VarRef v : rs) =
  writeDecRefsLnAcc (i + 1) (v : acc) rs
writeDecRefsLnAcc i acc (R.ConstRef _ : rs) =
  writeDecRefsLnAcc i acc rs

notHiddenConstExpr :: R.ConstExpr -> Bool
notHiddenConstExpr (R.Hidden _) = False
notHiddenConstExpr _ = True

notExternConstExpr :: R.ConstExpr -> Bool
notExternConstExpr (R.Extern _) = False
notExternConstExpr _ = True

writeProgram :: R.Program -> FilePath -> [FilePath] -> GenCode ()
writeProgram p hpath himports = do
  let cs = IntMap.elems p
  writeHPrelude himports
  writeCPrelude hpath
  writeDecls cs
  newLine CFile
  newLine CFile
  evalStateT (writeTrivialCtors cs) IntSet.empty
  evalStateT (writePapClosures cs) Set.empty
  writeConsts cs
  where
    writeDecls :: [(R.Const, R.ConstExpr)] -> GenCode ()
    writeDecls [] = return ()
    writeDecls [(c, e)] = writeDecl c e >> return ()
    writeDecls ((c, e) : cs) = do
      when (R.nameConst c /= Str.unitName) $ do
        writeDecl c e
        when (notHiddenConstExpr e) $ newLine CFile >> newLine CFile
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
      when (R.nameConst c /= Str.unitName) $ do
        writeConst c e
        when (notHiddenConstExpr e && notExternConstExpr e) $
          newLine CFile >> newLine CFile
      writeConsts cs

writeHPrelude :: [FilePath] -> GenCode ()
writeHPrelude ps = do
  writeStr HFile "#pragma once" >> newLine HFile
  writeStr HFile "#include <yu.h>" >> newLine HFile
  mapM_ (\p -> writeStr HFile ("#include \"" ++ p ++ "\"") >> newLine HFile) ps
  newLine HFile

writeCPrelude :: FilePath -> GenCode ()
writeCPrelude headerPath = do
  writeStr CFile ("#include \"" ++ headerPath ++ "\"")
  newLine CFile >> newLine CFile

writeTrivialCtorsConstExpr :: R.ConstExpr -> StateT IntSet GenCode ()
writeTrivialCtorsConstExpr (R.Hidden _) = return ()
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

writeTrivialCtorDecl :: FF -> R.Ctor -> GenCode ()
writeTrivialCtorDecl ff c =
  when (R.prCtor c /= 0) $ do
    writeStr ff "static "
    writeStructYuRef ff >> writeStr ff (" " ++ trivialCtorName c);

writeTrivialCtor :: R.Ctor -> GenCode ()
writeTrivialCtor c = do
  when (R.prCtor c /= 0) $ do
    writeTrivialCtorDecl CFile c
    writeStr CFile (" = {") >> newLine CFile
    writeStr CFile ("  .count = 0,") >> newLine CFile
    writeStr CFile ("  .tag = " ++ show (R.prCtor c) ++ ",") >> newLine CFile
    writeStr CFile ("  .vmt_index = yur_Static_vmt,") >> newLine CFile
    writeStr CFile ("  .nfields = 0") >> newLine CFile
    writeStr CFile ("};") >> newLine CFile
    newLine CFile

writePapClosuresConstExpr ::
  R.ConstExpr -> StateT (Set (R.Const, Int)) GenCode ()
writePapClosuresConstExpr (R.Hidden _) = return ()
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
    when (isJust v) $
      lift (writeLazyFun False c n >> newLine CFile >> newLine CFile)

papFunClosureParamName :: Int -> String
papFunClosureParamName i = 'a' : show i

writePapFunClosureDecl ::
  FF -> R.Const -> Maybe R.Var -> [R.Ref] -> Int -> GenCode ()
writePapFunClosureDecl ff c _v _rs n = do
  a <- constArity c
  let nparams = a - n
  when (n /= 0) (writeStr ff "static ")
  writeStructYuRefPtr ff
  writeStr ff (papClosureName c n)
  writeStr ff "("
  writeParams nparams 
  writeStr ff ")"
  where
    writeParams :: Int -> GenCode ()
    writeParams 0 = writeStructYuRefPtr ff >> writeStr ff "self"
    writeParams i = do
      writeStructYuRefPtr ff >> writeStr ff (papFunClosureParamName i)
      writeStr ff ", "
      writeParams (i-1)

writePapFunClosure :: R.Const -> Maybe R.Var -> [R.Ref] -> Int -> GenCode ()
writePapFunClosure c v rs n = do
  let recIdx = case v of
                Nothing -> Nothing
                Just v' -> List.elemIndex (R.VarRef v') rs
  a <- constArity c
  let nparams = a - n
  writePapFunClosureDecl CFile c v rs n
  writeStr CFile " {"
  incIndent >> newLine CFile
  writeTempsAndIncs [] recIdx 0
  if isNothing v
  then writeStr CFile "yur_unref_1(self);" >> newLine CFile
  else return ()
  writeStr CFile "return "
  writeStr CFile (funName c)
  writeStr CFile "("
  writeArgs nparams
  when (n > 0 && nparams > 0) (writeStr CFile ", ")
  writeAppliedArgs recIdx n
  writeStr CFile ");"
  decIndent >> newLine CFile
  writeStr CFile "}" >> newLine CFile
  newLine CFile
  where
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

    writeArgs :: Int -> GenCode ()
    writeArgs 0 = return ()
    writeArgs 1 = writeStr CFile (papFunClosureParamName 1)
    writeArgs i = do
      writeStr CFile (papFunClosureParamName i)
      writeStr CFile ", "
      writeArgs (i-1)

    writeFieldEntry :: Maybe Int -> Int -> GenCode ()
    writeFieldEntry recIdx i =
      if isJust v
      then writeStr CFile ("self->fields[" ++ show (convertIdx recIdx i + 1) ++ "]")
      else writeStr CFile ("self->fields[" ++ show i ++ "]")

    writeTempsAndIncs :: [String] -> Maybe Int -> Int -> GenCode ()
    writeTempsAndIncs incNames recIdx i =
      if i >= n
      then writeIncs 0 [] incNames
      else do
        if recIdx == Just i
        then writeTempsAndIncs ("self" : incNames) recIdx (i+1)
        else do
          writeStructYuRefPtr CFile
          let b = tempName recIdx i
          writeStr CFile (b ++ " = ")
          writeFieldEntry recIdx i
          writeStr CFile ";" >> newLine CFile
          writeTempsAndIncs (b : incNames) recIdx (i+1)

    writeIncs :: Int -> [String] -> [String] -> GenCode ()
    writeIncs 6 acc bs = do
      writeStr CFile "yur_inc_6("
      writeStringArgsList acc
      writeStr CFile ");"
      newLine CFile
      writeIncs 0 [] bs
    writeIncs _ [] [] = return ()
    writeIncs i acc [] = do
      writeStr CFile ("yur_inc_" ++ show i ++ "(")
      writeStringArgsList acc
      writeStr CFile ");"
      newLine CFile
    writeIncs i acc (b : bs) = do
      writeIncs (i + 1) (b : acc) bs

    writeStringArgsList :: [String] -> GenCode ()
    writeStringArgsList [] = error "writeStringArgsList expects at least one string"
    writeStringArgsList [s] = writeStr CFile s
    writeStringArgsList (s:ss) =
      writeStr CFile s >> writeStr CFile ", " >> writeStringArgsList ss

    writeAppliedArgs :: Maybe Int -> Int -> GenCode ()
    writeAppliedArgs _ 0 = return ()
    writeAppliedArgs recIdx 1 = writeStr CFile (tempName recIdx 0)
    writeAppliedArgs recIdx i = do
      writeStr CFile (tempName recIdx (i-1) ++ ", ")
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

writeFunDecl :: FF -> R.Const -> [R.Var] -> GenCode ()
writeFunDecl ff c vs = do
  writeStructYuRefPtr ff
  writeStr ff (funName c)
  writeParenVars ff vs

writeFunImplDecl :: FF -> R.Const -> GenCode ()
writeFunImplDecl ff c = do
  writeStructYuRef ff >> writeStr ff " " >> writeStr ff (funImpl c)

writeFunImpl :: R.Const -> GenCode ()
writeFunImpl c = do
  writeFunImplDecl CFile c
  writeStr CFile (" = {") >> newLine CFile
  writeStr CFile ("  .count = 0,") >> newLine CFile
  writeStr CFile ("  .tag = (size_t) &" ++ papClosureName c 0 ++ ",") >> newLine CFile
  writeStr CFile ("  .vmt_index = yur_Static_vmt,") >> newLine CFile
  writeStr CFile ("  .nfields = 0") >> newLine CFile
  writeStr CFile ("};");

writeLazyFunDecl :: FF -> Bool -> R.Const -> Int -> GenCode ()
writeLazyFunDecl ff _isStatic c n = do
  when (n /= 0) (writeStr ff "static ")
  writeStructYuRef ff >> writeStr ff " " >> writeStr ff (lazyFun c n)

writeLazyFun :: Bool -> R.Const -> Int -> GenCode ()
writeLazyFun isStatic c n = do
  writeLazyFunDecl CFile isStatic c n
  writeStr CFile (" = {") >> newLine CFile
  writeStr CFile ("  .count = 0,") >> newLine CFile
  writeStr CFile ("  .tag = (size_t) &" ++ papClosureName c n ++ ",") >> newLine CFile
  writeStr CFile ("  .vmt_index = yur_Static_vmt,") >> newLine CFile
  writeStr CFile ("  .nfields = 0") >> newLine CFile
  writeStr CFile ("};")

writeLazyImplDecl :: FF -> R.Const -> GenCode ()
writeLazyImplDecl ff c = do
  writeStructYuRef ff >> writeStr ff " " >> writeStr ff (funImpl c)

writeLazyImpl :: R.Const -> GenCode ()
writeLazyImpl c = do
  writeLazyImplDecl CFile c
  writeStr CFile (" = {") >> newLine CFile
  writeStr CFile ("  .count = 0,") >> newLine CFile
  writeStr CFile ("  .tag = 0,") >> newLine CFile
  writeStr CFile ("  .vmt_index = yur_Static_vmt,") >> newLine CFile
  writeStr CFile ("  .nfields = 1,") >> newLine CFile
  writeStr CFile ("  .fields[0] = (yur_Ref *) &" ++ lazyFun c 0) >> newLine CFile
  writeStr CFile ("};")

writeDecl :: R.Const -> R.ConstExpr -> GenCode ()
writeDecl _ (R.Hidden _) = return ()
writeDecl c (R.Extern vs) = do
  writeFunDecl HFile c vs
  writeHStr ";" >> newLine HFile >> newLine HFile
  writePapFunClosureDecl HFile c Nothing [] 0
  writeHStr ";" >> newLine HFile >> newLine HFile
  writePapFunClosure c Nothing [] 0
  writeStr HFile "extern "
  writeFunImplDecl HFile c
  writeHStr ";" >> newLine HFile >> newLine HFile
  writeFunImpl c
writeDecl c (R.Fun vs _) = do
  writeFunDecl HFile c vs
  writeHStr ";" >> newLine HFile >> newLine HFile
  writePapFunClosureDecl HFile c Nothing [] 0
  writeHStr ";" >> newLine HFile >> newLine HFile
  writePapFunClosure c Nothing [] 0
  writeStr HFile "extern "
  writeFunImplDecl HFile c
  writeHStr ";" >> newLine HFile >> newLine HFile
  writeFunImpl c
writeDecl c (R.Lazy iss vs _) = do
  writeFunDecl HFile c vs
  writeHStr ";" >> newLine HFile >> newLine HFile
  when iss $ do
    writePapFunClosureDecl HFile c Nothing [] 0
    writeHStr ";" >> newLine HFile >> newLine HFile
    writePapFunClosure c Nothing [] 0
    writeStr HFile "extern "
    writeLazyFunDecl HFile True c 0
    writeHStr ";" >> newLine HFile >> newLine HFile
    writeLazyFun True c 0
    newLine CFile >> newLine CFile
    writeStr HFile "extern "
    writeLazyImplDecl HFile c
    writeHStr ";" >> newLine HFile >> newLine HFile
    writeLazyImpl c

writeConst :: R.Const -> R.ConstExpr -> GenCode ()
writeConst _ (R.Hidden _) = return ()
writeConst _ (R.Extern _) = return ()
writeConst c (R.Fun vs e) = writeFun c vs e
writeConst c (R.Lazy _ vs e) = writeFun c vs e

writeFun :: R.Const -> [R.Var] -> R.FunExpr -> GenCode ()
writeFun c vs e = do
  writeFunDecl CFile c vs >> writeStr CFile " {"
  incIndent >> newLine CFile
  writeFunExpr e
  decIndent
  newLine CFile
  writeStr CFile "}"

writeRefTag :: R.Ref -> GenCode ()
writeRefTag (R.VarRef v) = writeVar CFile v >> writeStr CFile "->tag"
writeRefTag (R.ConstRef r) = writeStr CFile (funImpl r) >> writeStr CFile ".tag"

writeFunExprCases :: [(R.CtorId, R.FunExpr)] -> GenCode ()
writeFunExprCases [] = error "missing case"
writeFunExprCases [(_, e)] = do
  newLine CFile
  writeStr CFile ("default: {")
  incIndent >> newLine CFile
  writeFunExpr e
  decIndent >> newLine CFile
  writeStr CFile "}"
writeFunExprCases ((i, e) : cs) = do
  newLine CFile
  writeStr CFile ("case " ++ show i ++ ": {")
  incIndent >> newLine CFile
  writeFunExpr e
  decIndent >> newLine CFile
  writeStr CFile "}"
  writeFunExprCases cs

writeFunExpr :: R.FunExpr -> GenCode ()
writeFunExpr (R.Case r []) =
  writeStr CFile "yur_panic(\"expected case %zu to be unbreachable\", "
    >> writeRefTag r >> writeStr CFile ");"
writeFunExpr (R.Case r cs) = do
  writeStr CFile "switch (" >> writeRefTag r >> writeStr CFile ") {"
  writeFunExprCases cs
  newLine CFile
  writeStr CFile "}"
writeFunExpr (R.Pforce _ v rs e2) = do
  let rs' = filter (/= R.VarRef v) rs
  writeVar CFile v >> writeStr CFile "->nfields = "
    >> writeStr CFile (show (length rs' + 1)) >> writeStr CFile ";" >> newLine CFile
  setVarFields v 1 rs' >> newLine CFile
  writeFunExpr e2
writeFunExpr (R.Let v e1 e2) = do
  case e1 of
    R.Reset _ -> return ()
    _ -> writeVarDecl CFile v >> writeStr CFile ";" >> newLine CFile
  writeLetExpr v e1 >> newLine CFile
  writeFunExpr e2
writeFunExpr (R.Ret r) =
  writeStr CFile "return " >> writeRef CFile r >> writeStr CFile ";"
writeFunExpr (R.Inc v e) = do
  let acc = case v of
              R.VarRef w -> [w]
              R.ConstRef _ -> []
  writeFunExprIncs (length acc) acc e
writeFunExpr (R.Dec v e) = do
  let acc = case v of
              R.VarRef w -> [w]
              R.ConstRef _ -> []
  writeFunExprDecs (length acc) acc e
writeFunExpr (R.Unuse v e) = do
  writeStr CFile "yur_dealloc(" >> writeRef CFile v >> writeStr CFile ");" >> newLine CFile
  writeFunExpr e

writeVarArgsList :: [R.Var] -> GenCode ()
writeVarArgsList [] = error "writeVarArgsList expects at least one variable"
writeVarArgsList [v] = writeVar CFile v
writeVarArgsList (v:vs) = writeVarArgsList vs >> writeStr CFile ", " >> writeVar CFile v

writeFunExprIncs :: Int -> [R.Var] -> R.FunExpr -> GenCode ()
writeFunExprIncs 6 acc e = do
  writeStr CFile "yur_inc_6(" >> writeVarArgsList acc >> writeStr CFile ");" >> newLine CFile
  writeFunExpr e
writeFunExprIncs i acc (R.Inc v e) =
  case v of
    R.VarRef w -> writeFunExprIncs (i + 1) (w : acc) e
    R.ConstRef _ -> writeFunExprIncs i acc e
writeFunExprIncs 0 _ e =
  writeFunExpr e
writeFunExprIncs i acc e = do
  writeStr CFile ("yur_inc_" ++ show i ++ "(")
  writeVarArgsList acc
  writeStr CFile ");"
  newLine CFile
  writeFunExpr e

writeFunExprDecs :: Int -> [R.Var] -> R.FunExpr -> GenCode ()
writeFunExprDecs 6 acc e = do
  writeStr CFile "yur_unref_6(" >> writeVarArgsList acc >> writeStr CFile ");" >> newLine CFile
  writeFunExpr e
writeFunExprDecs i acc (R.Dec v e) =
  case v of
    R.VarRef w -> writeFunExprDecs (i + 1) (w : acc) e
    R.ConstRef _ -> writeFunExprDecs i acc e
writeFunExprDecs 0 _ e =
  writeFunExpr e
writeFunExprDecs i acc e = do
  writeStr CFile ("yur_unref_" ++ show i ++ "(")
  writeVarArgsList acc
  writeStr CFile ");"
  newLine CFile
  writeFunExpr e

writeFunType :: Int -> GenCode ()
writeFunType args = do
  writeStructYuRefPtr CFile >> writeStr CFile "(*)("
  doWrite args
  writeStr CFile ")"
  where
    doWrite :: Int -> GenCode ()
    doWrite 0 = return ()
    doWrite 1 = writeStructYuRefPtr CFile
    doWrite i = writeStructYuRefPtr CFile >> writeStr CFile ", " >> doWrite (i-1)

writeFunCast :: Int -> GenCode ()
writeFunCast args =
  writeStr CFile "(" >> writeFunType args >> writeStr CFile ")"

setFields :: String -> Int -> [R.Ref] -> GenCode ()
setFields _ _ [] = return ()
setFields prefix i (s:ss) = do
  writeStr CFile prefix
  writeStr CFile ("fields[" ++ show i ++ "] = ")
  writeRef CFile s
  writeStr CFile ";"
  when (not (null ss)) (newLine CFile)
  setFields prefix (i+1) ss

setVarFields :: R.Var -> Int -> [R.Ref] -> GenCode ()
setVarFields v = setFields (varName v ++ "->")

writeLetExpr :: R.Var -> R.LetExpr -> GenCode ()
writeLetExpr letVar (R.MkLazy True _ _) = do
  writeVar CFile letVar >> writeStr CFile (" = yur_alloc(1);") >> newLine CFile
  writeStr CFile "yur_init(" >> writeVar CFile letVar >> writeStr CFile ", 0, 0);"
  newLine CFile
  writeVar CFile letVar >> writeStr CFile "->fields[0] = 0;"
writeLetExpr letVar (R.MkLazy False _ c) = do
  a <- constArity c
  writeVar CFile letVar
  writeStr CFile (" = yur_alloc(" ++ show (a+1) ++ ");") >> newLine CFile
  writeStr CFile "yur_init(" >> writeVar CFile letVar >> writeStr CFile ", 1, 0);" >> newLine CFile
  writeVar CFile letVar >> writeStr CFile "->fields[0] = ("
  writeStructYuRefPtr CFile >> writeStr CFile ") "
  writeStr CFile (lazyRef c a) >> writeStr CFile ";"
writeLetExpr letVar (R.Ap _ (R.ConstRef r) rs) = do
  writeStr CFile (varName letVar) >> writeStr CFile " = ("
  writeFunCast (length rs) >> writeStr CFile (funName r) >> writeStr CFile ")"
  writeParenRefs CFile rs
  writeStr CFile ";"
writeLetExpr letVar (R.Ap _ (R.VarRef r) rs) = do
  writeStr CFile (varName letVar) >> writeStr CFile " = ("
  writeFunCast (length rs + 1) >> writeStr CFile " " >> writeRefTag (R.VarRef r)
  writeStr CFile ")("
  writeRefs CFile rs
  when (not (null rs)) (writeStr CFile ", ")
  writeStr CFile (varName r)
  writeStr CFile ");"
writeLetExpr letVar (R.Pap c rs) = do
  let n = length rs
  writeStr CFile (varName letVar)
    >> writeStr CFile
          (" = yur_build("
               ++ show n ++ ", "
               ++ "(size_t) &" ++ papClosureName c n ++ ");")
  newLine CFile
  setVarFields letVar 0 rs
writeLetExpr letVar (R.Force _ (R.ConstRef r) rs) = do
  writeStr CFile (varName letVar) >> writeStr CFile " = "
  writeStr CFile "yur_ALOAD("
    >> writeStr CFile (funImpl r) >> writeStr CFile ".fields[0]);"
  newLine CFile
  writeStr CFile "if (yur_ALOAD(" >> writeStr CFile (funImpl r) >> writeStr CFile ".tag)) {"
  incIndent >> newLine CFile
  writeDecRefsLn (R.forceProjArgs rs)
  decIndent >> newLine CFile
  writeStr CFile "} else {"
  incIndent >> newLine CFile
  writeStructYuRefPtr CFile >> writeStr CFile "expect = "
    >> writeStr CFile (varName letVar) >> writeStr CFile ";" >> newLine CFile
  writeStr CFile (varName letVar) >> writeStr CFile " = " >> writeStr CFile (funName r)
    >> writeParenRefs CFile (R.forceProjArgs rs) >> writeStr CFile ";"
  newLine CFile
  writeStr CFile "yur_memoize(" >> writeStr CFile (funRef r) >> writeStr CFile ", &"
    >> writeStr CFile (funImpl r) >> writeStr CFile ".fields[0], &"
    >> writeStr CFile (varName letVar) >> writeStr CFile ", expect);" >> newLine CFile
  writeStr CFile "yur_ASTORE(" >> writeStr CFile (funImpl r) >> writeStr CFile ".tag, 1);"
  decIndent >> newLine CFile
  writeStr CFile "}" >> newLine CFile
  writeStr CFile "yur_inc_1(" >> writeVar CFile letVar >> writeStr CFile ");"
writeLetExpr letVar (R.Force _ (R.VarRef r) Nothing) = do
  writeStr CFile (varName letVar) >> writeStr CFile " = "
  writeStr CFile "yur_ALOAD("
    >> writeStr CFile (varName r) >> writeStr CFile "->fields[0]);"
  writeStr CFile "if (!yur_ALOAD(" >> writeVar CFile r >> writeStr CFile "->tag)) {"
  incIndent >> newLine CFile
  writeStructYuRefPtr CFile >> writeStr CFile "expect = "
    >> writeStr CFile (varName letVar) >> writeStr CFile ";" >> newLine CFile
  writeStr CFile (varName letVar) >> writeStr CFile " = ("
  writeFunCast 1 >> writeStr CFile " "
    >> writeStr CFile (varName letVar) >> writeStr CFile "->tag)("
    >> writeStr CFile (varName r) >> writeStr CFile ");" >> newLine CFile
  writeStr CFile "yur_memoize(" >> writeVar CFile r >> writeStr CFile ", &"
    >> writeStr CFile (varName r) >> writeStr CFile "->fields[0], &"
    >> writeStr CFile (varName letVar) >> writeStr CFile ", expect);" >> newLine CFile
  writeStr CFile "yur_ASTORE(" >> writeVar CFile r >> writeStr CFile "->tag, 1);"
  decIndent >> newLine CFile
  writeStr CFile "}" >> newLine CFile
  writeStr CFile "yur_inc_1(" >> writeVar CFile letVar >> writeStr CFile ");"
writeLetExpr letVar (R.Force _ (R.VarRef r) (Just (c, rs))) = do
  writeStr CFile (varName letVar) >> writeStr CFile " = "
  writeStr CFile "yur_ALOAD("
    >> writeStr CFile (varName r) >> writeStr CFile "->fields[0]);"
  newLine CFile
  writeStr CFile "if (yur_ALOAD(" >> writeVar CFile r >> writeStr CFile "->tag)) {"
  incIndent >> newLine CFile
  writeDecRefsLn rs
  decIndent >> newLine CFile
  writeStr CFile "} else {"
  incIndent >> newLine CFile
  writeStructYuRefPtr CFile >> writeStr CFile "expect = "
    >> writeStr CFile (varName letVar) >> writeStr CFile ";" >> newLine CFile
  writeStr CFile (varName letVar) >> writeStr CFile " = "
  writeStr CFile (funName c)
    >> writeStr CFile "(" >> writeRefs CFile rs >> writeStr CFile ");" >> newLine CFile
  writeStr CFile "yur_ASTORE(" >> writeVar CFile r >> writeStr CFile "->nfields, 1);" >> newLine CFile
  writeStr CFile "yur_memoize(" >> writeVar CFile r >> writeStr CFile ", &"
    >> writeStr CFile (varName r) >> writeStr CFile "->fields[0], &"
    >> writeStr CFile (varName letVar) >> writeStr CFile ", expect);" >> newLine CFile
  writeStr CFile "yur_ASTORE(" >> writeVar CFile r >> writeStr CFile "->tag, 1);"
  decIndent >> newLine CFile
  writeStr CFile "}" >> newLine CFile
  writeStr CFile "yur_inc_1(" >> writeVar CFile letVar >> writeStr CFile ");"
writeLetExpr letVar (R.CtorBox c []) = do
  writeStr CFile (varName letVar)
  writeStr CFile " = "
  writeStr CFile (trivialCtorRef c)
  writeStr CFile ";"
writeLetExpr letVar (R.CtorBox c rs@(_:_)) = do
  let n = length rs
  writeStr CFile (varName letVar)
  writeStr CFile (" = yur_build(" ++ show n ++ ", " ++ show (R.prCtor c) ++ ");")
  newLine CFile
  setVarFields letVar 0 rs
writeLetExpr letVar (R.Proj i r) = do
  writeStr CFile (varName letVar)
  writeStr CFile " = ("
  writeRef CFile r
  writeStr CFile (")->fields[" ++ show i ++ "];")
writeLetExpr letVar (R.Reset n) = do
  writeVar CFile letVar
  writeStr CFile " = "
  writeStr CFile "yur_reset(" >> writeVar CFile letVar
    >> writeStr CFile ", " >> writeStr CFile (show n) >> writeStr CFile ");"
writeLetExpr letVar (R.Reuse v c rs) = do
  writeStr CFile (varName letVar) >> writeStr CFile " = "
    >> writeVar CFile v >> writeStr CFile ";" >> newLine CFile
  writeStr CFile (varName letVar) >> writeStr CFile "->tag = "
    >> writeStr CFile (show (R.prCtor c)) >> writeStr CFile ";" >> newLine CFile
  setVarFields letVar 0 rs
