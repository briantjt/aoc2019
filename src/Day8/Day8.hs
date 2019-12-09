module Day8.Day8 where

import           Lib
import           Data.List
import           Data.List.Split
import qualified Data.List.Key                 as K

day8Part1 :: IO ()
day8Part1 = do
  contents <- fileContents "/src/Day8/input.txt"
  let layers = chunksOf (25 * 6) contents
  print $ (\x -> count '1' x * count '2' x) $ K.minimum (count '0') layers


day8Part2 :: IO ()
day8Part2 = do
  contents <- fileContents "/src/Day8/input.txt"
  let layers = map (chunksOf 25) $ chunksOf (25 * 6) contents
      alignedLayers = map transpose $ transpose layers
  putStr $ unlines $ map (concatMap printPixel) alignedLayers

count :: Char -> String -> Int
count c s = count' c s 0 where
  count' :: Char -> String -> Int -> Int
  count' _ []       i = i
  count' c (x : xs) i = if c == x then count' c xs (i + 1) else count' c xs i

printPixel :: String -> String
printPixel [x] = case x of
  '1' -> "#"
  '0' -> " "
printPixel (x:xs) = case x of
  '2' -> printPixel xs
  '1' -> "#"
  '0' -> " "