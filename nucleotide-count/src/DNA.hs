module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Monad

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

toNucleotide :: Char -> Either String Nucleotide
toNucleotide n
  | n `elem` "ACGT" = Right ( read [n] :: Nucleotide)
  | otherwise       = Left ("invalid char: " ++ [n])
  
initialCountList :: [(Nucleotide, Int)]
initialCountList = [(A,0),(C,0),(G,0),(T,0)]

countUp :: [(Nucleotide, Int)] -> Char -> Either String [(Nucleotide, Int)]
countUp countList n = case toNucleotide n of
                        Left message -> Left message
                        Right x -> Right $ map (inc x) countList
                        where inc = \n' (nuc, count) -> if n' == nuc
                                                         then (nuc, count + 1)
                                                         else (nuc, count)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts n = fmap Map.fromList $ foldM countUp initialCountList n
