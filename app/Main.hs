{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

#include "split-stack-config.h"

module Main (main) where

import PackageMap (PackageMap)
import System.Console.ArgParser.Params as ArPa
import System.Console.ArgParser as Ar
import TypeCheck.TypeCheck
import qualified Ir.BaseIr as Ba
import qualified Ir.HighLevelIr as Hl
import qualified Ir.RefCountIr as Rc
import System.Directory (canonicalizePath)
import Control.Monad
import qualified System.Exit as Exit
import qualified Ir.CodeGen as Cg
import System.Command (command_)
import System.IO (hPutStrLn, stderr)
import System.FilePath (takeDirectory, takeFileName)
import Str (quote, stdRuntimePath)
import System.Environment (getArgs, getExecutablePath)
import qualified Data.Map as Map
import Data.IORef

collectIr :: IORef [FilePath] -> TypeCheckContCollect
collectIr objectFilesRef outputBaseName = do
  let ofile = outputBaseName ++ ".o"
  modifyIORef objectFilesRef (ofile :)

runIr :: IORef [FilePath] -> ProgramOptions -> TypeCheckContCompile
runIr objectFilesRef opts outputBaseName importBaseNames allBaseNames
        moduleName vs dm im rm =
  when (optionPrintHighLevelIR opts
        || optionPrintBaseIR opts
        || optionPrintRefCountIR opts
        || optionAssembly opts
        || optionCompile opts)
    runFromHighLevelIr
  where
    runFromHighLevelIr :: IO ()
    runFromHighLevelIr = do
      let (hap, har) = Hl.highLevelIr (not $ optionFast opts) moduleName vs dm im rm
      when (optionPrintHighLevelIR opts) $ do
        putStrLn "\n## High level intermediate representation\n"
        putStrLn (Hl.irToString hap har)
      when (optionPrintBaseIR opts
            || optionPrintRefCountIR opts
            || optionAssembly opts
            || optionCompile opts) $
        runFromBaseIr hap har

    runFromBaseIr :: Hl.Program -> Hl.ProgramRoots -> IO ()
    runFromBaseIr hap har = do
      bap <- Ba.baseIr (not $ optionFast opts) (optionVerboseOutput opts)
              outputBaseName allBaseNames moduleName hap har
      when (optionPrintBaseIR opts) $ do
        putStrLn "\n## Base intermediate representation\n"
        putStrLn (Ba.irToString bap)
      when (optionPrintRefCountIR opts
            || optionAssembly opts
            || optionCompile opts) $
        runFromRefCountIr bap

    runFromRefCountIr :: Ba.Program -> IO ()
    runFromRefCountIr bap = do
      let rcp = Rc.refCountIr bap
      when (optionPrintRefCountIR opts) $ do
        putStrLn "\n## Reference counted intermediate representation\n"
        putStrLn (Rc.irToString rcp)
      when (optionAssembly opts
            || optionCompile opts) $ do
        runFromAssemblyCompile rcp

    runFromAssemblyCompile :: Rc.Program -> IO ()
    runFromAssemblyCompile rcp = do
      let cfile = outputBaseName ++ ".c"
      let hfile = outputBaseName ++ ".h"
      let himports = map (++ ".h") importBaseNames
      Cg.genCode rcp cfile hfile himports
      let root = projectRootPath opts
      let runtimePath = root ++ "/" ++ stdRuntimePath
      let mimallocLib = root ++ "/mimalloc/out/libmimalloc.a"
      let gcc = if optionNoSplitStack opts
                then "gcc"
                else root ++ "/gcc/yu-stack-install/bin/gcc"
      when (optionAssembly opts) $ do
        let outfile = outputBaseName ++ ".s"
        putStrLn $ "assemble " ++ outfile
        let cargs = if optionOptimize opts
                    then ["-std=gnu11",
                           "-Wall",
                           "-pthread",
                           "-S",
                           "-O3",
                           "-DNDEBUG",
                           "-momit-leaf-frame-pointer",
                           "-I", runtimePath,
                           "-o", outfile,
                           cfile]
                    else ["-std=gnu11",
                           "-Wall",
                           "-pthread",
                           "-S",
                           "-O1",
                           "-foptimize-sibling-calls",
                           "-momit-leaf-frame-pointer",
                           "-I", runtimePath,
                           "-o", outfile,
                           cfile]
        let debugOpt = if optionDebug opts then ["-g"] else []
        let splitArgs = if optionNoSplitStack opts
                        then ["-Dyur_DISABLE_SPLIT_STACK"]
                        else ["-fyu-stack", "-fno-omit-frame-pointer"]
        let allArgs = splitArgs ++ cargs ++ debugOpt ++ argumentGccOptions opts
        when (optionVerboseOutput opts) $
          putStrLn (gcc ++ concat (map (" "++) allArgs))
        command_ [] gcc allArgs
      when (optionCompile opts) $ do
        let runtime = if optionNoSplitStack opts
                      then runtimePath ++ "/out/system-stack/libyur.a"
                      else runtimePath ++ "/out/split-stack/libyur.a"
        let link = moduleName == ""
        let outfile = if link
                      then outputBaseName ++ ".exe"
                      else outputBaseName ++ ".o"
        objectFiles <- readIORef objectFilesRef
        if link
        then do
          putStrLn $ "build and link " ++ outfile
        else do
          writeIORef objectFilesRef (outfile : objectFiles)
          putStrLn $ "build " ++ outfile
        let compileArg = if link then [] else ["-c"]
        let cargs = if optionOptimize opts
                    then ["-std=gnu11",
                           "-Wall",
                           "-pthread",
                           "-DNDEBUG",
                           "-O3",
                           "-mtune=native",
                           "-momit-leaf-frame-pointer",
                           "-I", runtimePath,
                           "-o", outfile,
                           cfile]
                    else ["-std=gnu11",
                           "-Wall",
                           "-pthread",
                           "-O1",
                           "-foptimize-sibling-calls",
                           "-momit-leaf-frame-pointer",
                           "-I", runtimePath,
                           "-o", outfile,
                           cfile]
        let linkArgs = if not link
                       then []
                       else
                         objectFiles ++
                          (if not $ optionFast opts then ["-Wl,-s"] else []) ++
                          ["-static",
                           runtime,
                           mimallocLib,
                           "-Wl,--whole-archive",
                           "-lpthread",
                           "-Wl,--no-whole-archive",
                           "-latomic"]
        let debugOpt = if optionDebug opts then ["-g"] else []
        let splitArgs = if optionNoSplitStack opts
                        then ["-Dyur_DISABLE_SPLIT_STACK"]
                        else ["-fyu-stack", "-fno-omit-frame-pointer"]
        let allArgs = compileArg
                      ++ splitArgs
                      ++ cargs
                      ++ debugOpt
                      ++ linkArgs
                      ++ argumentGccOptions opts
        when (optionVerboseOutput opts) $
          putStrLn (gcc ++ concat (map (" "++) allArgs))
        command_ [] gcc allArgs

getProjectPath :: IO FilePath
getProjectPath = do
  e <- getExecutablePath
  canonicalizePath (takeDirectory e ++ "/..")

packagePaths :: ProgramOptions -> IO PackageMap
packagePaths opts = do
  let yuPath = projectRootPath opts ++ "/stdlib/yu"
  aps <- mapM canonicalizePath (optionPackages opts)
  let ps = map (\ p -> (takeFileName p, p)) aps
  return (Map.fromList (("yu", yuPath) : ps))

run :: ProgramOptions -> IO ()
run opts = do
  packs <- packagePaths opts
  let params = TypeCheckParams
                { tcParamVerbose = optionVerboseOutput opts
                , tcParamCompile = optionCompile opts || optionAssembly opts
                , tcParamClean = optionClean opts
                }
  objectFilesRef <- newIORef []
  tc <- runTT params packs (argumentFileName opts)
          (runIr objectFilesRef opts, collectIr objectFilesRef)
  case tc of
    Just msg -> hPutStrLn stderr msg >> Exit.exitWith (Exit.ExitFailure 1)
    Nothing -> return ()

data ProgramOptions = ProgramOptions
  { optionPackages :: [String]
  , optionCompile :: Bool
  , optionClean :: Bool
  , optionAssembly :: Bool
  , optionOptimize :: Bool
  , optionFast :: Bool
  , optionDebug :: Bool
  , optionNoSplitStack :: Bool
  , optionPrintHighLevelIR :: Bool
  , optionPrintBaseIR :: Bool
  , optionPrintRefCountIR :: Bool
  , optionVerboseOutput :: Bool
  , projectRootPath :: FilePath
  , argumentFileName :: FilePath
  , argumentGccOptions :: [FilePath]
  }

makeProgramOptions ::
  FilePath -> [FilePath] -> [String] ->
  Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool ->
  ProgramOptions
makeProgramOptions
  argFileName
  argGccOptions
  optPackages
  optCompile
  optClean
  optAssembly
  optOptimize
  optFast
  optDebug
  optNoSplitStack
  optPrintHighLevelIR
  optPrintBaseIR
  optPrintRefCountIR
  optVerboseOutput
  = ProgramOptions
    { optionPackages = optPackages
    , optionCompile = optCompile
    , optionClean = optClean
    , optionAssembly = optAssembly
    , optionOptimize = optOptimize
    , optionFast = optFast
    , optionDebug = optDebug
    , optionNoSplitStack = NO_SPLIT_STACK || optNoSplitStack
    , optionPrintHighLevelIR = optPrintHighLevelIR
    , optionPrintBaseIR = optPrintBaseIR
    , optionPrintRefCountIR = optPrintRefCountIR
    , optionVerboseOutput = optVerboseOutput
    , argumentFileName = argFileName
    , argumentGccOptions = argGccOptions
    , projectRootPath = ""
    }

cmdParser :: Ar.ParserSpec ProgramOptions
cmdParser = makeProgramOptions
  `Ar.parsedBy` reqPos "file"
    `Ar.Descr` "The mini-yu source code file."
  `Ar.andBy` posArgs "gcc files" [] (\xs x -> xs ++ [x])
    `Ar.Descr` "Additional files passed to gcc."
  `Ar.andBy` Ar.optFlagArgs [] "package" [] (\ ps p -> p : ps)
    `Ar.Descr` "Paths of Mini Yu packages to include."
  `Ar.andBy` ArPa.FlagParam ArPa.Short "compile" id
    `Ar.Descr` "Compile the source code."
  `Ar.andBy` ArPa.FlagParam ArPa.Long "clean" id
    `Ar.Descr` "Force compiler to re-type-check and re-compile everything."
  `Ar.andBy` ArPa.FlagParam ArPa.Short "assembly" id
    `Ar.Descr` "Output compiler generated assembly code."
  `Ar.andBy` ArPa.FlagParam ArPa.Short "optimize" id
    `Ar.Descr` "Enable more than the default optimizations to improve performance."
  `Ar.andBy` ArPa.FlagParam ArPa.Long "fast" id
    `Ar.Descr` "Disable optimizations to build faster."
  `Ar.andBy` ArPa.FlagParam ArPa.Long "debug" id
    `Ar.Descr` "Enable gcc debug information."
  `Ar.andBy` ArPa.FlagParam ArPa.Long "no-split-stack" id
    `Ar.Descr` "Use a large (3GB) system stack instead of split (segmented) stack."
  `Ar.andBy` ArPa.FlagParam ArPa.Long "print-high-level-ir" id
    `Ar.Descr` "Print initial intermediate representation."
  `Ar.andBy` ArPa.FlagParam ArPa.Long "print-base-ir" id
    `Ar.Descr` "Print second intermediate representation."
  `Ar.andBy` ArPa.FlagParam ArPa.Long "print-ref-count-ir" id
    `Ar.Descr` "Print reference counted intermediate representation."
  `Ar.andBy` ArPa.FlagParam ArPa.Long "verbose" id
    `Ar.Descr` "Enable verbose output."

verifyOptions :: ProgramOptions -> IO ()
verifyOptions opts
  | optionFast opts && optionOptimize opts = do
      hPutStrLn stderr ("Options " ++ quote "--fast" ++ " and "
                        ++ quote "--optimize" ++ " are incompatible together")
      Exit.exitWith (Exit.ExitFailure 1)
  | True = return ()

cmdInterface :: IO (Ar.CmdLnInterface ProgramOptions)
cmdInterface = mkApp cmdParser

main :: IO ()
main = do
  interface <- cmdInterface
  args <- getArgs
  case parseArgs args interface of
    Right opts -> preRun opts >>= run
    Left msg -> do
      if msg == "too many arguments"
      then hPutStrLn stderr ("Invalid command line arguments, try " ++ quote "--help")
      else hPutStrLn stderr msg
      Exit.exitWith (Exit.ExitFailure 1)
  where
    preRun :: ProgramOptions -> IO ProgramOptions
    preRun opts = do
      verifyOptions opts
      a <- canonicalizePath (argumentFileName opts)
      p <- getProjectPath
      return (opts { argumentFileName = a
                   , projectRootPath = p })
