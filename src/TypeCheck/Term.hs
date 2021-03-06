module TypeCheck.Term
  ( Var (..)
  , VarId
  , VarIdSet
  , VarName
  , SubstMap
  , CaseTree (..)
  , caseTreeHasIo
  , PreTerm (..)
  , RefVar (..)
  , DataCtorMap
  , Term (..)
  , mkTerm
  , PrePattern (..)
  , Pattern (..)
  , RefMap
  , RefMeta (..)
  , ImplicitMap
  , ExternSet
  , Implicits
  , mkVar
  , mkTermTy
  , mkTermUnitTy
  , mkTermUnitElem
  , prePatternIsVar
  , prePatternToPreTerm
  , prePatternGetRoot
  , preTermIsRigid
  )
where

import Loc (Loc)
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type VarId = Int

type VarName = String

data Var = Var {varId :: VarId, varName :: VarName} deriving Show

type SubstMap = IntMap PreTerm

            -- Implicit names, Bool = True if IO fun,
            -- number of explicit arguments, and case tree:
data PreTerm = TermFun [VarName] Bool (Maybe Int) CaseTree
            -- Lazy fun Bool = True if IO fun:
             | TermLazyFun Bool PreTerm
                -- Arrow Bool = True if it is an IO arrow:
             | TermArrow Bool [(Maybe Var, PreTerm)] PreTerm
                -- Lazy arrow Bool = True if it is an IO arrow:
             | TermLazyArrow Bool PreTerm
                -- Application Bool = True if IO application:
             | TermApp Bool PreTerm [PreTerm]
                -- Bool for implicit app is true if the implicit app
                -- has been given explicitly:
             | TermImplicitApp Bool PreTerm [(VarName, PreTerm)]
                -- Lazy aplication where Bool = True if IO application:
             | TermLazyApp Bool PreTerm
             | TermRef Var SubstMap -- Val name and lazy substitution.
             | TermVar Bool Var     -- Variable, Bool = True if implicit.
             | TermData Var         -- Data type definition with ctors
             | TermCtor Var VarId   -- Data constructor with data type
             | TermCase PreTerm CaseTree
             | TermUnitElem
             | TermUnitTy
             | TermTy
             | TermEmpty  -- Just for converting pattern to term
             deriving Show

data RefVar = RefExtern Var Int | RefVal Var | RefData Var | RefCtor Var deriving Show

type DataCtorMap = IntMap [Var] -- Data type VarId -> ctors

data CaseTree =
    CaseLeaf
      [Var]      -- [Var] to substitute remaining arguments
      Bool       -- Bool = True if leaf has IO effect.
      PreTerm
      [RefVar]   -- where-clause (only used for code generation)
  | CaseNode
      -- Which argument to pattern match on.
      Int
      -- Constructor cases VarId -> ([Var], CaseTree)
      -- The [Var] is indicating which variables to substitute
      -- the term for.
      (IntMap ([Var], CaseTree))
      (Maybe ([Var], CaseTree)) -- Catch all case.
  | CaseEmpty Int
  | CaseUnit
      Int      -- Which argument of type Unit to pattern match on.
      ([Var], CaseTree) -- Next case tree.
  deriving Show

data Term = Term
  { termPre :: PreTerm
  , termTy :: PreTerm
  , termIo :: Bool
  , termNestedDefs :: [RefVar] } deriving Show

mkTerm :: PreTerm -> PreTerm -> Bool -> Term
mkTerm tp tt io =
  Term {termPre = tp, termTy = tt, termIo = io, termNestedDefs = []}

-- A pattern should have been:
-- PatternApp CTOR PATTERN...
-- PatternRoot ROOT_PATTERN...
-- where ROOT_PATTERN cannot be an application.
--   and CTOR is just the var id of a ctor.
data PrePattern = PatternApp PrePattern [PrePattern]
                  -- Bool for implicit app is true if the implicit app
                  -- has been given explicitly.
                | PatternImplicitApp Bool PrePattern [(VarName, PrePattern)]
                | PatternLazyApp PrePattern
                | PatternCtor Var VarId
                | PatternVar Var
                | PatternUnit
                | PatternEmpty
                deriving Show

data Pattern =
  Pattern { patternPre :: PrePattern
          , patternTy :: PreTerm } deriving Show

type Implicits = [(Var, PreTerm)]

type ImplicitMap = IntMap Implicits

data RefMeta = RefMeta
  { refMetaIsTerminationChecked :: Bool
  , refMetaIsDeclaredPure :: Bool
  , refMetaLoc :: Loc
  , refMetaName :: VarName
  } deriving Show

type ExternSet = IntSet

type RefMap = IntMap (Term, RefMeta)

type VarIdSet = IntSet

-------------------------------------------------------------------------

caseTreeHasIo :: CaseTree -> Bool
caseTreeHasIo (CaseLeaf _ io _ _) = io
caseTreeHasIo (CaseNode _ m d) =
  any (caseTreeHasIo . snd) (IntMap.elems m)
  || case d of
      Nothing -> False
      Just (_, x) -> caseTreeHasIo x
caseTreeHasIo (CaseEmpty _) = False
caseTreeHasIo (CaseUnit _ (_, x)) = caseTreeHasIo x

mkVar :: VarId -> VarName -> Var
mkVar i n = Var {varId = i, varName = n}

mkTermTy :: Term
mkTermTy = mkTerm TermTy TermTy False

mkTermUnitTy :: Term
mkTermUnitTy = mkTerm TermUnitTy TermTy False

mkTermUnitElem :: Term
mkTermUnitElem = mkTerm TermUnitElem TermUnitTy False

prePatternIsVar :: PrePattern -> Bool
prePatternIsVar (PatternVar _) = True
prePatternIsVar _ = False

prePatternToPreTerm :: PrePattern -> PreTerm
prePatternToPreTerm (PatternApp a as) =
  TermApp False (prePatternToPreTerm a) (map prePatternToPreTerm as)
prePatternToPreTerm (PatternLazyApp a) =
  TermLazyApp False (prePatternToPreTerm a)
prePatternToPreTerm (PatternImplicitApp b a as) =
  TermImplicitApp b (prePatternToPreTerm a) (map implicitToPreTerm as)
  where
    implicitToPreTerm :: (VarName, PrePattern) -> (VarName, PreTerm)
    implicitToPreTerm (n, p) = (n, prePatternToPreTerm p)
prePatternToPreTerm (PatternVar v) = TermVar False v
prePatternToPreTerm (PatternCtor v i) = TermCtor v i
prePatternToPreTerm PatternUnit = TermUnitElem
prePatternToPreTerm PatternEmpty = TermEmpty

prePatternGetRoot :: PrePattern -> PrePattern
prePatternGetRoot (PatternApp f _) = prePatternGetRoot f
prePatternGetRoot (PatternLazyApp f) = prePatternGetRoot f
prePatternGetRoot (PatternImplicitApp _ f _) = prePatternGetRoot f
prePatternGetRoot r = r

preTermIsRigid :: PreTerm -> Bool
preTermIsRigid (TermApp _ f _) = preTermIsRigid f
preTermIsRigid (TermImplicitApp _ f _) = preTermIsRigid f
preTermIsRigid (TermLazyApp _ f) = preTermIsRigid f
preTermIsRigid (TermCtor _ _) = True
preTermIsRigid (TermData _) = True
preTermIsRigid (TermArrow _ _ _) = True
preTermIsRigid (TermLazyArrow _ _) = True
preTermIsRigid TermTy = True
preTermIsRigid TermUnitTy = True
preTermIsRigid TermUnitElem = True
preTermIsRigid _ = False
