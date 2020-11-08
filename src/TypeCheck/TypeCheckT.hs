module TypeCheck.TypeCheckT
  ( TypeCheckT
  , TypeCheckErr (..)
  , typeCheckErrMsg
  , err
  , catchRecoverable
  , ImpMap (..)
  , impMapTopSet
  , ExprT
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
import Data.Set (Set)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)

data TypeCheckErr =
  Fatal String | Recoverable String

typeCheckErrMsg :: TypeCheckErr -> String
typeCheckErrMsg (Fatal s) = s
typeCheckErrMsg (Recoverable s) = s

type TypeCheckT m =
  Env.EnvT (StateT (Set FilePath) (ReaderT FilePath (ExceptT TypeCheckErr m)))

err :: Monad m => Loc -> TypeCheckErr -> TypeCheckT m a
err lo msg = do
  f <- ask
  case msg of
    Fatal msg' ->
      throwError (Fatal (f ++ ":" ++ show lo ++ ": " ++ msg'))
    Recoverable msg' ->
      throwError (Recoverable (f ++ ":" ++ show lo ++ ": " ++ msg'))

data ImpMap = ImpMap (IntMap (Loc, String)) (Maybe ImpMap)

impMapTopSet :: ImpMap -> IntSet
impMapTopSet (ImpMap m _) = IntMap.keysSet m

type ExprT m = StateT (SubstMap, ImpMap) (TypeCheckT m)

catchRecoverable :: Monad m => ExprT m a -> (String -> ExprT m a) -> ExprT m a
catchRecoverable t f = catchError t continueWithRecoverable
  where
    continueWithRecoverable (Recoverable m) = f m
    continueWithRecoverable e@(Fatal _) = throwError e

getExprSubst :: Monad m => ExprT m SubstMap
getExprSubst = fmap fst get

modifyExprSubst :: Monad m => (SubstMap -> SubstMap) -> ExprT m ()
modifyExprSubst f = modify (\(s, x) -> (f s, x))

putExprSubst :: Monad m => SubstMap -> ExprT m ()
putExprSubst s = modifyExprSubst (const s)

impMapInsert :: Monad m => VarId -> PreTerm -> Loc -> String -> ExprT m ()
impMapInsert i t lo msg = do
  lift (Env.forceInsertImplicitVar i t)
  modify (\(s, ImpMap m p) ->
              (s, ImpMap (IntMap.insert i (lo, msg) m) p))

defaultExprIndent :: Int
defaultExprIndent = 2
