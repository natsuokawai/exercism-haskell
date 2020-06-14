module CollatzConjecture (collatz) where

type Count = Integer

collatz :: Integer -> Maybe Count 
collatz num
  | num < 1   = Nothing
  | otherwise = fst <$> (subCollatz 0 num)

subCollatz :: Count -> Integer -> Maybe (Count, Integer)
subCollatz cnt 1 = Just (cnt, 1)
subCollatz cnt num
  | odd num      = subCollatz (cnt + 1) (3 * num + 1)
  | even num     = subCollatz (cnt + 1) (num `div` 2)
subCollatz _ _   = Nothing
