module TypeCheck.HackGlobalVarId
  ( hackGlobalVarId
  , hackFreshGlobalVarId
  , hackReadGlobalVarId
  ) where

import TypeCheck.Term
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.IORef

{- Hack global state because the substitution and normalisation
 - implementations are too naive. The performence of these dependent
 - on lazy evaluation, which is preserved by this global state hack. -}

hackGlobalVarId :: IORef VarId
hackGlobalVarId = unsafeDupablePerformIO (newIORef 0)
{-# NOINLINE hackGlobalVarId #-}

hackFreshGlobalVarId :: a -> (VarId, a)
hackFreshGlobalVarId x =
  unsafeDupablePerformIO $ do
    i <- readIORef hackGlobalVarId
    writeIORef hackGlobalVarId (i + 1)
    return (i, x)
{-# NOINLINE hackFreshGlobalVarId #-}

hackReadGlobalVarId :: a -> (VarId, a)
hackReadGlobalVarId x = do
  unsafeDupablePerformIO $ do
    i <- readIORef hackGlobalVarId
    return (i, x)
{-# NOINLINE hackReadGlobalVarId #-}

