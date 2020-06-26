module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

toNucleotide :: Char -> Either String Nucleotide
toNucleotide n
  | n `elem` "ACGT" = Right ( read [n] :: Nucleotide)
  | otherwise       = Left ("invalid char: " ++ [n])
  
initialCountList :: Map Nucleotide Int
initialCountList = Map.fromList [(A,0),(C,0),(G,0),(T,0)]

countUp :: Map Nucleotide Int -> Char -> Either String (Map Nucleotide Int)
countUp countList n = pure (Map.adjust (+1)) <*> (toNucleotide n) <*> Right countList

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldM countUp initialCountList xs

