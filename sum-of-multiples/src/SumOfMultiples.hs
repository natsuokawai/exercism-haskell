module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum [ num | num <- [0..(limit - 1)], isMultipleOf factorsWithoutZero num]
  where factorsWithoutZero = filter (/= 0) factors

isMultipleOf :: [Integer] -> Integer -> Bool
isMultipleOf xs x = any (== 0) $ map (rem x) xs

