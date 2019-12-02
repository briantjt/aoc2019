module Day1.Day1 where

import           System.IO
import           System.Directory

day1 :: IO Int
day1 = do
  root     <- getCurrentDirectory
  contents <- readFile (root ++ "/src/Day1/input.txt")
  return
    $ sum
    $ map ((\x -> x + calculateFuelForFuel x) . calculateFuel . read)
    $ lines contents

calculateFuel :: Int -> Int
calculateFuel weight = (weight `quot` 3) - 2

calculateFuelForFuel :: Int -> Int
calculateFuelForFuel fuel =
  let nextFuel = calculateFuel fuel
  in  if nextFuel > 0 then nextFuel + calculateFuelForFuel nextFuel else 0
