module Day4.Day4 where

import qualified Data.Map                      as M
numbers :: [Int]
numbers = [197487 .. 673251]

splitDigits :: Int -> [Int]
splitDigits i | i < 10    = [i]
              | otherwise = splitDigits (i `quot` 10) ++ [i `mod` 10]

twoSameDigits :: [Int] -> Bool
twoSameDigits []           = False
twoSameDigits [x         ] = False
twoSameDigits (x : y : zs) = (x == y) || twoSameDigits (y : zs)

increasingOrder :: [Int] -> Bool
increasingOrder []           = True
increasingOrder [a         ] = True
increasingOrder (x : y : zs) = (x <= y) && increasingOrder (y : zs)

part1passwords :: [[Int]]
part1passwords =
  filter (\x -> twoSameDigits x && increasingOrder x) $ map splitDigits numbers

day4part1 = length part1passwords

-- Count the number of digits in each password and check if there is a digit
-- with a count of exactly 2
day4part2 = length $ filter (any (\(a, b) -> b == 2)) $ map
  (M.toList . M.fromListWith (+) . (\x -> zip x (repeat (1 :: Int))))
  part1passwords
