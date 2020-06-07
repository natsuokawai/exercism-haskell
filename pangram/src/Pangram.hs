module Pangram (isPangram) where

import qualified Data.Char as C
import qualified Data.Set as S

alphaList :: String
alphaList = ['a'..'z']

isPangram :: String -> Bool
isPangram text = S.isSubsetOf (S.fromList alphaList) (S.fromList $ map (C.toLower) text)
