{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

#include "split-stack-config.h"

module Main where

import PackageMap (PackageMap)
import System.Console.ArgParser.Params as ArPa
import System.Console.ArgParser as Ar
import qualified TypeCheck.Term as Te
import TypeCheck.TypeCheck
import qualified Ir.BaseIr as Ba
import qualified Ir.HighLevelIr as Hl
import qualified Ir.RefCountIr as Rc
import System.Directory (makeAbsolute)
import Control.Monad
import qualified System.Exit as Exit
import qualified Ir.CodeGen as Cg
import System.Command (command_)
import System.IO (hPutStrLn, stderr)
import System.FilePath (takeDirectory, takeFileName)
import Str (stdRuntimePath)
import System.Environment (getArgs, getExecutablePath)
import qualified Data.Map as Map

runIr ::
  ProgramOptions -> [Te.RefVar] -> Te.DataCtorMap -> Te.ImplicitMap -> Te.RefMap -> IO ()
runIr opts vs dm im rm = do
  let (hap, har) = Hl.highLevelIr (optionOptimize opts) vs dm im rm
  when (optionPrintHighLevelIR opts) $ do
    putStrLn "\n## High level intermediate representation\n"
    putStrLn (Hl.irToString hap har)
  let needsMain = optionCompile opts || optionAssembly opts
  let bap = Ba.baseIr (optionOptimize opts) needsMain hap har
  when (optionPrintBaseIR opts) $ do
    putStrLn "\n## Base intermediate representation\n"
    putStrLn (Ba.irToString bap)
  let rcp = Rc.refCountIr bap
  when (optionPrintRefCountIR opts) $ do
    putStrLn "\n## Reference counted intermediate representation\n"
    putStrLn (Rc.irToString rcp)
  let cfile = argumentFileName opts ++ ".c"
  when (optionAssembly opts || optionCompile opts) $ do
    Cg.genCode rcp cfile
  let root = projectRootPath opts
  let runtimePath = root ++ "/" ++ stdRuntimePath
  let mimallocLib = root ++ "/mimalloc/out/libmimalloc.a"
  let gcc = if optionNoSplitStack opts
            then "gcc"
            else root ++ "/gcc/yu-stack-install/bin/gcc"
  when (optionAssembly opts) $ do
    let outfile = argumentFileName opts ++ ".s"
    let cargs = if optionOptimize opts
                then ["-std=gnu11",
                       "-Wall",
                       "-static",
                       "-S",
                       "-O3",
                       "-DNDEBUG",
                       "-momit-leaf-frame-pointer",
                       "-I", runtimePath,
                       "-o", outfile,
                       cfile]
                else ["-std=gnu11",
                       "-Wall",
                       "-static",
                       "-S",
                       "-g",
                       "-momit-leaf-frame-pointer",
                       "-I", runtimePath,
                       "-o", outfile,
                       cfile]
    let splitArgs = if optionNoSplitStack opts
                    then ["-Dyur_DISABLE_SPLIT_STACK"]
                    else ["-fyu-stack", "-fno-omit-frame-pointer"]
    let allArgs = splitArgs ++ cargs ++ argumentGccOptions opts
    when (optionVerboseOutput opts) $
      putStrLn (gcc ++ concat (map (" "++) allArgs))
    command_ [] gcc allArgs
  when (optionCompile opts) $ do
    let runtime = if optionNoSplitStack opts
                  then runtimePath ++ "/out/system-stack/libyur.a"
                  else runtimePath ++ "/out/split-stack/libyur.a"
    let outfile = argumentFileName opts ++ ".exe"
    let cargs = if optionOptimize opts
                then ["-std=gnu11",
                       "-Wall",
                       "-static",
                       "-DNDEBUG",
                       "-O3",
                       "-mtune=native",
                       "-momit-leaf-frame-pointer",
                       "-I", runtimePath,
                       "-o", outfile,
                       cfile,
                       "-Wl,--whole-archive",
                       runtime,
                       "-Wl,--no-whole-archive",
                       mimallocLib,
                       "-Wl,--whole-archive",
                       "-lpthread",
                       "-Wl,--no-whole-archive",
                       "-latomic"]
                else ["-std=gnu11",
                       "-Wall",
                       "-static",
                       "-g",
                       "-momit-leaf-frame-pointer",
                       "-I", runtimePath,
                       "-o", outfile,
                       cfile,
                       "-Wl,--whole-archive",
                       runtime,
                       "-Wl,--no-whole-archive",
                       mimallocLib,
                       "-Wl,--whole-archive",
                       "-lpthread",
                       "-Wl,--no-whole-archive",
                       "-latomic"]
    let splitArgs = if optionNoSplitStack opts
                    then ["-Dyur_DISABLE_SPLIT_STACK"]
                    else ["-fyu-stack", "-fno-omit-frame-pointer"]
    let allArgs = splitArgs ++ cargs ++ argumentGccOptions opts
    when (optionVerboseOutput opts) $
      putStrLn (gcc ++ concat (map (" "++) allArgs))
    command_ [] gcc allArgs

getProjectPath :: IO FilePath
getProjectPath = do
  e <- getExecutablePath
  makeAbsolute (takeDirectory e ++ "/..")

getMainPath :: ProgramOptions -> IO FilePath
getMainPath opts = do
  let a = argumentFileName opts
  makeAbsolute (takeDirectory a)

packagePaths :: ProgramOptions -> IO PackageMap
packagePaths opts = do
  let yuPath = projectRootPath opts ++ "/stdlib/yu"
  aps <- mapM makeAbsolute (optionPackages opts)
  let ps = map (\ p -> (takeFileName p, p)) aps
  return (Map.fromList (("yu", yuPath) : ps))

run :: ProgramOptions -> IO ()
run opts = do
  verifyProgramOptions opts
  packs <- packagePaths opts
  tc <- runTT (optionVerboseOutput opts)
              (optionCompile opts || optionAssembly opts)
              packs
              (argumentFileName opts)
              (runIr opts)
  case tc of
    Just msg -> hPutStrLn stderr msg >> Exit.exitWith (Exit.ExitFailure 1)
    Nothing -> return ()

data ProgramOptions = ProgramOptions
  { optionPackages :: [String]
  , optionCompile :: Bool
  , optionAssembly :: Bool
  , optionOptimize :: Bool
  , optionNoSplitStack :: Bool
  , optionPrintHighLevelIR :: Bool
  , optionPrintBaseIR :: Bool
  , optionPrintRefCountIR :: Bool
  , optionVerboseOutput :: Bool
  , projectRootPath :: FilePath
  , argumentFileName :: FilePath
  , argumentGccOptions :: [FilePath]
  }

verifyProgramOptions :: ProgramOptions -> IO ()
verifyProgramOptions opts
  | optionOptimize opts && not (optionAssembly opts || optionCompile opts)
      = Exit.die $ "optimization option (-o, --optimize) requires at least one of\n\tcompilation option (-c, --compile)\n\tassembly option (-a, --assembly)"
  | optionNoSplitStack opts && not (optionAssembly opts || optionCompile opts)
      = Exit.die $ "disable split stack option (--no-split-stack) requires at least one of\n\tcompilation option (-c, --compile)\n\tassembly option (-a, --assembly)"
  | True = return ()

makeProgramOptions ::
  FilePath -> [FilePath] -> [String] -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool ->
  ProgramOptions
makeProgramOptions
  argFileName
  argGccOptions
  optPackages
  optCompile
  optAssembly
  optOptimize
  optNoSplitStack
  optPrintHighLevelIR
  optPrintBaseIR
  optPrintRefCountIR
  optVerboseOutput
  = ProgramOptions
    { optionPackages = optPackages
    , optionCompile = optCompile
    , optionAssembly = optAssembly
    , optionOptimize = optOptimize
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
    `Ar.Descr` "the mini-yu source code file"
  `Ar.andBy` posArgs "gcc files" [] (\xs x -> xs ++ [x])
    `Ar.Descr` "additional files passed to gcc"
  `Ar.andBy` Ar.optFlagArgs [] "package" [] (\ ps p -> p : ps)
    `Ar.Descr` "paths to include packages"
  `Ar.andBy` ArPa.FlagParam ArPa.Short "compile" id
    `Ar.Descr` "compile the source code"
  `Ar.andBy` ArPa.FlagParam ArPa.Short "assembly" id
    `Ar.Descr` "output compiler generated assembly"
  `Ar.andBy` ArPa.FlagParam ArPa.Short "optimize" id
    `Ar.Descr` "optimize to improve performance of generated program"
  `Ar.andBy` ArPa.FlagParam ArPa.Long "no-split-stack" id
    `Ar.Descr` "use a large (3GB) system stack instead of split (segmented) stack"
  `Ar.andBy` ArPa.FlagParam ArPa.Long "print-high-level-ir" id
    `Ar.Descr` "print initial intermediate representation"
  `Ar.andBy` ArPa.FlagParam ArPa.Long "print-base-ir" id
    `Ar.Descr` "print second intermediate representation"
  `Ar.andBy` ArPa.FlagParam ArPa.Long "print-ref-count-ir" id
    `Ar.Descr` "print reference counted intermediate representation"
  `Ar.andBy` ArPa.FlagParam ArPa.Long "verbose" id
    `Ar.Descr` "enable verbose output"

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
      then hPutStrLn stderr "invalid command line arguments, try again with just --help"
      else hPutStrLn stderr msg
      Exit.exitWith (Exit.ExitFailure 1)
  where
    preRun :: ProgramOptions -> IO ProgramOptions
    preRun opts = do
      a <- makeAbsolute (argumentFileName opts)
      p <- getProjectPath
      return (opts { argumentFileName = a
                   , projectRootPath = p })
