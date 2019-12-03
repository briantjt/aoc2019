module Day3.Day3 where

import           System.Directory
import           Data.List.Split
import           Data.Array.MArray
import           Data.Array.IO
import qualified Data.Set                      as S
import qualified Data.Map                      as M

day3 :: IO ()
day3 = do
  root  <- getCurrentDirectory
  input <- readFile (root ++ "/src/Day3/input.txt")
  let
    input1 : input2 : rest = lines input
    wire1 = map (\(x : xs) -> (x, read xs :: Int)) $ splitOn "," input1
    wire2 = map (\(x : xs) -> (x, read xs :: Int)) $ splitOn "," input2
    wire1Coords            = generateWire (0, 0) wire1
    wire2Coords            = generateWire (0, 0) wire2
    set1                   = S.fromList wire1Coords
    set2                   = S.fromList wire2Coords
    intersections          = set1 `S.intersection` set2
    map1                   = M.fromList $ zip wire1Coords [1 ..]
    map2                   = M.fromList $ zip wire2Coords [1 ..]
    costs =
      traverse (\key -> (+) <$> M.lookup key map1 <*> M.lookup key map2)
        $ S.toList intersections

  print $ minimum $ map (\(x, y) -> abs x + abs y) $ S.toList intersections
  print $ minimum <$> costs



generateCoords :: (Int, Int) -> (Char, Int) -> [(Int, Int)]
generateCoords (x, y) (direction, steps) = case direction of
  'L' -> (x - steps, y) : [ (x - x1, y) | x1 <- [1 .. steps] ]
  'R' -> (x + steps, y) : [ (x + x1, y) | x1 <- [1 .. steps] ]
  'U' -> (x, y + steps) : [ (x, y + y1) | y1 <- [1 .. steps] ]
  'D' -> (x, y - steps) : [ (x, y - y1) | y1 <- [1 .. steps] ]

generateWire :: (Int, Int) -> [(Char, Int)] -> [(Int, Int)]
generateWire _ [] = []
generateWire pos directions =
  let nextPos : wire = generateCoords pos (head directions)
  in  wire ++ generateWire nextPos (tail directions)
