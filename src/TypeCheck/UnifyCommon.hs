module TypeCheck.UnifyCommon
  ( makeFunWithVarSubst
  )
where

import TypeCheck.Env as Env
import TypeCheck.TypeCheckT
import TypeCheck.Term
import TypeCheck.SubstMap

import qualified Data.IntMap as IntMap

makeFunWithVarSubst :: Monad m => Bool -> [Var] -> PreTerm -> TypeCheckT m PreTerm
makeFunWithVarSubst isIo vs t = do
  vs' <- mapM (\v -> fmap (flip mkVar (varName v)) Env.freshVarId) vs
  let su = IntMap.fromList (zip (map varId vs) (map (TermVar False) vs'))
  let ct = CaseLeaf vs' isIo (substPreTerm su t) []
  return (TermFun [] isIo (Just (length vs)) ct)
