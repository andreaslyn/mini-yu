module Loc (Loc, loc, line, column, locToSourcePos)
where

import Text.Parsec.Pos (SourcePos, newPos)

data Loc = Loc {line :: Int, column :: Int} deriving Eq

instance Show Loc where
  show (Loc {line = r, column = c}) = show r ++ ":" ++ show c

instance Ord Loc where
  lo1 <= lo2 = line lo1 < line lo2
               || (line lo1 == line lo2 && column lo1 <= column lo2)

loc :: Int -> Int -> Loc
loc r c = Loc {line = r, column = c}

locToSourcePos :: FilePath -> Loc -> SourcePos
locToSourcePos p lo = newPos p (line lo) (column lo)
