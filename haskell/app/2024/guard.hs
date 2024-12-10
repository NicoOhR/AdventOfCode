{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Text.Printf (printf)

-- Pt 1 Plane
-- since the guard will always go (up, right, down, left)
-- 0. Base case: Guard is at edge of grid
-- 1. we start by finding the guards position
-- 2. we add the visited to a set, and if the next position is a #
-- we recur through an infinite directions list

type Point = (Int, Int)

type Visited = Set.Set Point

type Grid = Map.Map Point Char

makeGrid :: [String] -> Map.Map Point Char
makeGrid rows =
  Map.fromList [((r, c), char) | (r, row) <- zip [0 ..] rows, (c, char) <- zip [0 ..] row]

getVisited :: Point -> [Point -> Point] -> Visited -> Grid -> Visited
getVisited s d v g = loop s d v
  where
    loop current (direction : directions) visited
      | isNothing (Map.lookup (direction current) g) = visited
      | Map.lookup (direction current) g == Just '#' =
          loop current directions visited
      | Map.lookup (direction current) g == Just '.' || Map.lookup (direction current) g == Just '^' = if Set.member (direction current) visited then loop (direction current) (direction : directions) visited else loop (direction current) (direction : directions) (Set.insert (direction current) visited)
      | otherwise = error (printf "Something has gone terribly wrong")

directions :: [Point -> Point]
directions =
  cycle
    [ \(x, y) -> (x - 1, y),
      \(x, y) -> (x, y + 1),
      \(x, y) -> (x + 1, y),
      \(x, y) -> (x, y - 1)
    ]

findGuard :: Grid -> Point
findGuard g =
  fromMaybe (0, 0) (Map.lookup '^' $ Map.fromList [(v, k) | (k, v) <- Map.toList g])

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines_ = lines contents
  let grid = makeGrid lines_
  let directions_ = directions
  let starting = findGuard grid
  let visited = Set.singleton starting
  let finalVisited = getVisited starting directions_ visited grid
  let count = length finalVisited
  print count
