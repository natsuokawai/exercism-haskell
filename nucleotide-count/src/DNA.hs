module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

toNucleotide :: Char -> Either String Nucleotide
toNucleotide n
  | n `elem` "ACGT" = Right ( read [n] :: Nucleotide)
  | otherwise       = Left ("invalid char: " ++ [n])
  
nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = fmap Map.fromList $ pure zip
                      <*> (mapM toNucleotide $ List.nub sorted)
                      <*> pure (map length (List.group sorted))
                      where sorted = List.sort xs
