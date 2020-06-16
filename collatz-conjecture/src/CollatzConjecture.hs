module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz num
  | num <= 0  = Nothing
  | otherwise = Just $ fromIntegral $ length $ collatzList num

collatzList :: Integer -> [Integer]
collatzList x
  | x == 1 = []
  | odd x  = x:collatzList (3 * x + 1)
  | even x = x:collatzList (x `div` 2)
collatzList _ = error "Invalid argument."
