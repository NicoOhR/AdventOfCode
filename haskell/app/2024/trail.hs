import Data.Char (digitToInt)
import qualified Data.Map as Map

type Point = (Int, Int)

type Grid = Map.Map Point Int

-- I recognize a Graph is probably the better data structure
makeGrid :: [[Int]] -> Grid
makeGrid rows =
  Map.fromList [((r, c), height) | (r, row) <- zip [0 ..] rows, (c, height) <- zip [0 ..] row]

findHeads :: Grid -> [Point]
findHeads = Map.keys . Map.filter (== 0)

main :: IO ()
main = do
  contents <- readFile "test.txt"
  let lines_ = map (map digitToInt) $ lines contents
  let g = makeGrid lines_
  print g
