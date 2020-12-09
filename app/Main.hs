{-# LANGUAGE BangPatterns #-}

module Main where

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
import System.FilePath (takeDirectory)
import Str (stdRuntimePath)
import System.Environment (getExecutablePath)

runIr ::
  ProgramOptions -> [Te.RefVar] -> Te.DataCtorMap -> Te.ImplicitMap -> Te.RefMap -> IO ()
runIr opts vs dm im rm = do
  let (hap, har) = Hl.highLevelIr vs dm im rm
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
  let runtime = root ++ "/" ++ stdRuntimePath
  let mimallocInclude = root ++ "/mimalloc/include"
  let mimallocLib = root ++ "/mimalloc/out/libmimalloc.a"
  when (optionAssembly opts) $ do
    let outfile = argumentFileName opts ++ ".s"
    let cargs = if optionOptimize opts
                then ["-std=c11",
                       "-Wall",
                       "-S",
                       "-O3",
                       "-I", mimallocInclude,
                       "-I", runtime,
                       "-o", outfile,
                       cfile]
                else ["-std=c11",
                       "-Wall",
                       "-S",
                       "-g",
                       "-I", mimallocInclude,
                       "-I", runtime,
                       "-o", outfile,
                       cfile]
    command_ [] "gcc" (cargs ++ argumentGccOptions opts)
  when (optionCompile opts) $ do
    let cruntime = runtime ++ "/yu.c"
    let parruntime = runtime ++ "/parallel.c"
    let strruntime =  runtime++ "/yustr.c"
    let listruntime = runtime ++ "/yulist.c"
    let outfile = argumentFileName opts ++ ".exe"
    let cargs = if optionOptimize opts
                then ["-std=c11",
                       "-Wall",
                       "-O3",
                       "-I", mimallocInclude,
                       "-I", runtime,
                       "-o", outfile,
                       cfile, cruntime, parruntime, strruntime, listruntime,
                       mimallocLib,
                       "-lpthread",
                       "-latomic"]
                else ["-std=c11",
                       "-Wall",
                       "-g",
                       "-I", mimallocInclude,
                       "-I", runtime,
                       "-o", outfile,
                       cfile, cruntime, parruntime, strruntime, listruntime,
                       mimallocLib,
                       "-lpthread",
                       "-latomic"]
    command_ [] "gcc" (cargs ++ argumentGccOptions opts)

getProjectPath :: IO FilePath
getProjectPath = do
  e <- getExecutablePath
  makeAbsolute (takeDirectory e ++ "/..")

run :: ProgramOptions -> IO ()
run opts = do
  verifyProgramOptions opts
  tc <- runTT (optionVerboseErrors opts)
              (optionCompile opts || optionAssembly opts)
              (projectRootPath opts ++ "/stdlib/") (argumentFileName opts)
  case tc of
    Left msg -> hPutStrLn stderr msg
    Right (vs, dm, im, rm) -> runIr opts vs dm im rm

data ProgramOptions = ProgramOptions
  { optionCompile :: Bool
  , optionAssembly :: Bool
  , optionOptimize :: Bool
  , optionPrintHighLevelIR :: Bool
  , optionPrintBaseIR :: Bool
  , optionPrintRefCountIR :: Bool
  , optionVerboseErrors :: Bool
  , projectRootPath :: FilePath
  , argumentFileName :: FilePath
  , argumentGccOptions :: [FilePath]
  }

verifyProgramOptions :: ProgramOptions -> IO ()
verifyProgramOptions opts
  | optionOptimize opts && not (optionAssembly opts || optionCompile opts)
      = Exit.die $ "optimization option (-o, --optimize) requires at least one of\n\
                   \  compilation option (-c, --compile)\
                   \  assembly option (-a, --assembly)"
  | True = return ()

makeProgramOptions ::
  Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool ->
  FilePath -> [FilePath] -> ProgramOptions
makeProgramOptions
  optCompile
  optAssembly
  optOptimize
  optPrintHighLevelIR
  optPrintBaseIR
  optPrintRefCountIR
  optVerboseErrors
  argFileName
  argGccOptions
  = ProgramOptions
    { optionCompile = optCompile
    , optionAssembly = optAssembly
    , optionOptimize = optOptimize
    , optionPrintHighLevelIR = optPrintHighLevelIR
    , optionPrintBaseIR = optPrintBaseIR
    , optionPrintRefCountIR = optPrintRefCountIR
    , optionVerboseErrors = optVerboseErrors
    , argumentFileName = argFileName
    , argumentGccOptions = argGccOptions
    , projectRootPath = ""
    }

cmdParser :: Ar.ParserSpec ProgramOptions
cmdParser = makeProgramOptions
  `Ar.parsedBy` ArPa.FlagParam ArPa.Short "compile" id
    `Ar.Descr` "compile the source code"
  `Ar.andBy` ArPa.FlagParam ArPa.Short "assembly" id
    `Ar.Descr` "output compiler generated assembly"
  `Ar.andBy` ArPa.FlagParam ArPa.Short "optimize" id
    `Ar.Descr` "optimize to improve performance of generated program"
  `Ar.andBy` ArPa.FlagParam ArPa.Long "print-high-level-ir" id
    `Ar.Descr` "print initial intermediate representation"
  `Ar.andBy` ArPa.FlagParam ArPa.Long "print-base-ir" id
    `Ar.Descr` "print second intermediate representation"
  `Ar.andBy` ArPa.FlagParam ArPa.Long "print-ref-count-ir" id
    `Ar.Descr` "print reference counted intermediate representation"
  `Ar.andBy` ArPa.FlagParam ArPa.Long "verbose" id
    `Ar.Descr` "print error message verbosely"
  `Ar.andBy` reqPos "file"
    `Ar.Descr` "the mini-yu source code file"
  `Ar.andBy` posArgs "gcc files" [] (\xs x -> xs ++ [x])
    `Ar.Descr` "additional files passed to gcc"

cmdInterface :: IO (Ar.CmdLnInterface ProgramOptions)
cmdInterface = mkApp cmdParser

main :: IO ()
main = do
  interface <- cmdInterface
  runApp interface (\opts -> preRun opts >>= run)
  where
    preRun :: ProgramOptions -> IO ProgramOptions
    preRun opts = do
      a <- makeAbsolute (argumentFileName opts)
      p <- getProjectPath
      return (opts { argumentFileName = a
                   , projectRootPath = p })
