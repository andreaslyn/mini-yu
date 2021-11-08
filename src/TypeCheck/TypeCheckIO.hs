module TypeCheck.TypeCheckIO
  ( TypeCheckIO
  , TypeCheckErr (..)
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

data TypeCheckErr =
  Fatal String | Recoverable String

typeCheckErrMsg :: TypeCheckErr -> String
typeCheckErrMsg (Fatal s) = s
typeCheckErrMsg (Recoverable s) = s

type TypeCheckIO =
  Env.EnvT
    (StateT (Map FilePath (Maybe Bool, Env.ScopeMap))
      (ReaderT (Bool, FilePath, String)
        (ExceptT TypeCheckErr IO)))

err :: Loc -> TypeCheckErr -> TypeCheckIO a
err lo msg = do
  (_, f, _) <- ask
  case msg of
    Fatal msg' ->
      throwError (Fatal (f ++ ":" ++ show lo ++ ": " ++ msg'))
    Recoverable msg' ->
      throwError (Recoverable (f ++ ":" ++ show lo ++ ": " ++ msg'))

expandVarNameEnv :: VarName -> TypeCheckIO VarName
expandVarNameEnv n = do
  (_, _, modName) <- ask
  Env.expandVarName modName n

lookupEnv :: VarName -> TypeCheckIO (Maybe Env.VarStatus)
lookupEnv n = do
  (_, _, modName) <- ask
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
