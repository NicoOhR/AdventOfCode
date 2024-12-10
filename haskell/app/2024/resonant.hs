import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.List (groupBy, nub, sortOn)
import qualified Data.Map as M
import Data.Maybe (isNothing)

type Point = (Int, Int)

type Grid = M.Map Point Char

-- should make subclass whatever
subtract' :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
subtract' (a1, b1) (a2, b2) = (a1 - a2, b1 - b2)

add' :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
add' (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

scale :: (Num b) => b -> (b, b) -> (b, b)
scale a (a1, b1) = (a * a1, a * b1)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = [(x, y) | y <- xs] ++ [(y, x) | y <- xs] ++ pairs xs

makeGrid :: [String] -> Grid
makeGrid rows =
  M.fromList [((r, c), char) | (r, row) <- zip [0 ..] rows, (c, char) <- zip [0 ..] row]

findAntennas :: Grid -> [[Point]]
findAntennas g =
  let f = M.toList $ M.filter isAlphaNum g
      group = groupBy ((==) `on` snd) $ sortOn snd f
   in map (map fst) group

colinear :: (Point, Point) -> Point
colinear (x, y) = add' (subtract' x y) x

valid :: Grid -> Point -> Bool
valid g p
  | isNothing $ M.lookup p g =
      False
  | otherwise = True

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let grid = makeGrid $ lines contents
  let antennas = map pairs $ findAntennas grid
  let antinodes = map (valid grid) $ nub (concatMap (map colinear) antennas)
  let total = length $ filter id antinodes
  print total
