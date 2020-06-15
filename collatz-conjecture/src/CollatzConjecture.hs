module CollatzConjecture (collatz) where

type Count = Integer

collatz :: Integer -> Maybe Count
collatz num
  | num < 1 = Nothing
  | otherwise = Just $ fromIntegral $ length $ collatzList [num]

collatzList :: [Integer] -> [Integer]
collatzList (x:xs)
  | x == 1 = []
  | odd x  = (3 * x + 1):(collatzList xs)
  | even x = (x `div` 2):(collatzList xs)

