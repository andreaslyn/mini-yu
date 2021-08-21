module PackageMap
  ( PackageMap
  , takePackage
  ) where

import Data.Map (Map)

type PackageMap = Map String FilePath

takePackage :: String -> Maybe (String, FilePath)
takePackage s =
  let (p, s') = span (/= '/') s
  in
    if null s' || head s' /= '/'
    then Nothing
    else Just (p, s')
