module Day6.Day6 where

import           Debug.Trace
import           System.Directory
import           Data.List.Split
import qualified Data.Map.Strict               as M
import           Day4.Day4                      ( splitDigits )

type Planet = String

fileContents :: IO String
fileContents = do
  root <- getCurrentDirectory
  readFile (root ++ "/src/Day6/input.txt")


day6Part1 :: IO ()
day6Part1 = do
  input <- fileContents
  let orbits     = map ((\(x : y : _) -> (x, [y])) . splitOn ")") $ lines input
      orbitalMap = M.fromListWith (++) orbits
  print $ countOrbits orbitalMap 0 "COM"

day6Part2 :: IO ()
day6Part2 = do
  input <- fileContents
  let orbits     = map ((\(x : y : _) -> (x, [y])) . splitOn ")") $ lines input
      orbitalMap = M.fromListWith (++) orbits
      ancestor   = commonAncestor orbitalMap
      findOrbits g = orbitCount orbitalMap 0 g "COM"
      youOrbits      = head $ findOrbits "YOU"
      sanOrbits      = head $ findOrbits "SAN"
      ancestorOrbits = head $ findOrbits ancestor
  print $ youOrbits + sanOrbits - ((ancestorOrbits + 1) * 2)

countOrbits :: M.Map Planet [Planet] -> Int -> Planet -> Int
countOrbits orbitalMap count planet = case M.lookup planet orbitalMap of
  Just p  -> count + sum (map (countOrbits orbitalMap (count + 1)) p)
  Nothing -> count

findPath :: M.Map Planet [Planet] -> Planet -> Planet -> [Planet] -> [Planet]
findPath orbitalMap start goal currentPath =
  let planets = M.lookup start orbitalMap
  in  case planets of
        Just ps -> if goal `elem` ps
          then currentPath
          else concatMap
            (\p -> findPath orbitalMap p goal (currentPath ++ [p]))
            ps
        Nothing -> []

commonAncestor :: M.Map Planet [Planet] -> Planet
commonAncestor orbitalMap = getCommonAncestors youPath sanPath
 where
  youPath = findPath orbitalMap "COM" "YOU" []
  sanPath = findPath orbitalMap "COM" "SAN" []
  getCommonAncestors (x1 : y1 : z1) (x2 : y2 : z2) =
    if y1 /= y2 then x1 else getCommonAncestors (y1 : z1) (y2 : z2)


orbitCount :: M.Map Planet [Planet] -> Int -> Planet -> Planet -> [Int]
orbitCount orbitalMap count goal planet =
  let planets = M.lookup planet orbitalMap
  in  case planets of
        Just p -> if goal `elem` p
          then [count]
          else filter (> 0)
            $ concatMap (orbitCount orbitalMap (count + 1) goal) p
        Nothing -> [0]
