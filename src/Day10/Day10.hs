module Day10.Day10 where

import           Data.Complex
import qualified Data.Set                      as S
import           Data.List
import           Lib
import qualified Data.List.Key                 as K
import           Data.Ord

type Asteroid = Complex Float

toCoords :: [String] -> [(Float, Float)]
toCoords input =
  [ (x, y) | (row, y) <- zip input [0 ..], (s, x) <- zip row [0 ..], s == '#' ]

parse :: [(Float, Float)] -> [Asteroid]
parse coords = [ x :+ y | (x, y) <- coords ]

genPolarCoords :: [Asteroid] -> [([Complex Float], Complex Float)]
genPolarCoords asteroids = map
  (\asteroid1 ->
    ( filter (\x -> x /= 0 :+ 0)
      $ map (\asteroid2 -> asteroid2 - asteroid1) asteroids
    , asteroid1
    )
  )
  asteroids

highest :: [([Complex Float], Complex Float)] -> ([Complex Float], Complex Float)
highest = K.maximum (length . S.fromList . map normalizePhase . fst)

normalizePhase :: Complex Float -> Float
normalizePhase x =
  let y = phase x
  in  if y < (-0.5) * pi
        then 2.5 * pi + y
        else 0.5 * pi + y

day10 :: IO ()
day10 = do
  contents <- fileContents "/src/Day10/input.txt"
  let coords               = toCoords (lines contents)
      asteroids            = genPolarCoords $ parse coords
      (bestStation, index) = highest asteroids
      sortOrder = mconcat [comparing normalizePhase, comparing magnitude]
      rows                 = length $ head $ lines contents
      groupedAsteroids =
        groupBy (\x y -> normalizePhase x == normalizePhase y)
          $ sortBy sortOrder bestStation
  print $ length groupedAsteroids
  print $ index + (head $ groupedAsteroids !! 199)
