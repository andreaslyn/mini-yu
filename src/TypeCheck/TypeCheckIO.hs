module TypeCheck.TypeCheckIO
  ( TypeCheckParams (..)
  , TypeCheckIO
  , TypeCheckErr (..)
  , isVerboseOn
  , isCompileOn
  , isCleanOn
  , typeCheckParams
  , currentFilePath
  , currentModuleName
  , currentOutputFileBaseName
  , currentOutputObjectFileName
  , typeCheckErrMsg
  , err
  , lookupEnv
  , expandVarNameEnv
  , catchRecoverable
  , ImpMap (..)
  , impMapTopSet
  , ExprIO
  , getExprSubst
  , modifyExprSubst
  , putExprSubst
  , impMapInsert
  , defaultExprIndent
  ) where

import Loc (Loc)
import qualified TypeCheck.Env as Env
import TypeCheck.Term
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import Data.Time.Clock (UTCTime)

data TypeCheckParams =
  TypeCheckParams
    { tcParamVerbose :: Bool
    , tcParamCompile :: Bool
    , tcParamClean :: Bool
    }

data TypeCheckErr =
  Fatal String | Recoverable String

typeCheckErrMsg :: TypeCheckErr -> String
typeCheckErrMsg (Fatal s) = s
typeCheckErrMsg (Recoverable s) = s

type TypeCheckIO =
  Env.EnvT
    (StateT (Map FilePath (Maybe UTCTime, Env.ScopeMap))
      (ReaderT (TypeCheckParams, FilePath, VarName, FilePath)
        (ExceptT TypeCheckErr IO)))

{-# INLINE isVerboseOn #-}
isVerboseOn :: TypeCheckIO Bool
isVerboseOn = do
  (params, _, _, _) <- ask
  return (tcParamVerbose params)

{-# INLINE isCompileOn #-}
isCompileOn :: TypeCheckIO Bool
isCompileOn = do
  (params, _, _, _) <- ask
  return (tcParamCompile params)

{-# INLINE isCleanOn #-}
isCleanOn :: TypeCheckIO Bool
isCleanOn = do
  (params, _, _, _) <- ask
  return (tcParamClean params)

{-# INLINE typeCheckParams #-}
typeCheckParams :: TypeCheckIO TypeCheckParams
typeCheckParams = do
  (params, _, _, _) <- ask
  return params

{-# INLINE currentFilePath #-}
currentFilePath :: TypeCheckIO FilePath
currentFilePath = do
  (_, file, _, _) <- ask
  return file

{-# INLINE currentModuleName #-}
currentModuleName :: TypeCheckIO VarName
currentModuleName = do
  (_, _, modName, _) <- ask
  return modName

{-# INLINE currentOutputFileBaseName #-}
currentOutputFileBaseName :: TypeCheckIO FilePath
currentOutputFileBaseName = do
  (_, _, _, outputBase) <- ask
  return outputBase

currentOutputObjectFileName :: TypeCheckIO FilePath
currentOutputObjectFileName = do
  base <- currentOutputFileBaseName
  return (base ++ ".o")

err :: Loc -> TypeCheckErr -> TypeCheckIO a
err lo msg = do
  f <- currentFilePath
  case msg of
    Fatal msg' ->
      throwError (Fatal (f ++ ":" ++ show lo ++ ": " ++ msg'))
    Recoverable msg' ->
      throwError (Recoverable (f ++ ":" ++ show lo ++ ": " ++ msg'))

expandVarNameEnv :: VarName -> TypeCheckIO VarName
expandVarNameEnv n = do
  modName <- currentModuleName
  Env.expandVarName modName n

lookupEnv :: VarName -> TypeCheckIO (Maybe Env.VarStatus)
lookupEnv n = do
  modName <- currentModuleName
  Env.lookup modName n

data ImpMap = ImpMap (IntMap (Loc, String)) (Maybe ImpMap)

impMapTopSet :: ImpMap -> IntSet
impMapTopSet (ImpMap m _) = IntMap.keysSet m

type ExprIO = StateT (SubstMap, ImpMap) TypeCheckIO

catchRecoverable :: ExprIO a -> (String -> ExprIO a) -> ExprIO a
catchRecoverable t f = catchError t continueWithRecoverable
  where
    continueWithRecoverable (Recoverable m) = f m
    continueWithRecoverable e@(Fatal _) = throwError e

getExprSubst :: ExprIO SubstMap
getExprSubst = fmap fst get

modifyExprSubst :: (SubstMap -> SubstMap) -> ExprIO ()
modifyExprSubst f = modify (\(s, x) -> (f s, x))

putExprSubst :: SubstMap -> ExprIO ()
putExprSubst s = modifyExprSubst (const s)

impMapInsert :: VarId -> PreTerm -> Loc -> String -> ExprIO ()
impMapInsert i t lo msg = do
  lift (Env.insertImplicitVarCurrentScope i t)
  modify (\(s, ImpMap m p) ->
              (s, ImpMap (IntMap.insert i (lo, msg) m) p))

defaultExprIndent :: Int
defaultExprIndent = 2
