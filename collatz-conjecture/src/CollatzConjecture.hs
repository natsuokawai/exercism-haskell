module CollatzConjecture (collatz) where

type Count = Integer

collatz :: Integer -> Maybe Count 
collatz num
  | num < 1   = Nothing
  | otherwise = Just $ subCollatz 0 num

subCollatz :: Count -> Integer -> Count
subCollatz cnt 1 = cnt
subCollatz cnt num
  | odd num      = subCollatz (cnt + 1) (3 * num + 1)
  | even num     = subCollatz (cnt + 1) (num `div` 2)
subCollatz _ _   = error "Invalid argument"
