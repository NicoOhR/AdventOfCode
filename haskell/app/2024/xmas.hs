import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Text.Regex.TDFA.Text ()

type Point = (Int, Int)

type Grid = Map.Map Point Char

makeGrid :: [String] -> Map.Map Point Char
makeGrid rows =
  Map.fromList [((r, c), char) | (r, row) <- zip [0 ..] rows, (c, char) <- zip [0 ..] row]

count g = sum . map (\p -> countXmas g p stepFunctions) . findX $ g

countXmas g p = length . filter (isXmas g p)

isXmas :: Grid -> Point -> (Point -> Point) -> Bool
isXmas g p steps = word == "XMAS"
  where
    word = catMaybes . take 4 . map (`Map.lookup` g) . iterate steps $ p

findX :: Grid -> [Point]
findX = Map.keys . Map.filter (== 'X')

findA :: Grid -> [Point]
findA = Map.keys . Map.filter (== 'A')

countCross :: Grid -> Int
countCross grid = length . filter (isCross grid) . findA $ grid

isCross :: Grid -> Point -> Bool
isCross g (x, y) = current `elem` valid
  where
    lookup' c = fromMaybe ' ' (Map.lookup c $ g)
    current = [[lookup' (x - 1, y + 1), ' ', lookup' (x + 1, y + 1)], [' ', 'A', ' '], [lookup' (x - 1, y - 1), ' ', lookup' (x + 1, y - 1)]]
    valid =
      [ ["M S", " A ", "M S"],
        ["S M", " A ", "S M"],
        ["M M", " A ", "S S"],
        ["S S", " A ", "M M"]
      ]

stepFunctions :: [Point -> Point]
stepFunctions =
  [ \(x, y) -> (x - 1, y - 1),
    \(x, y) -> (x - 1, y),
    \(x, y) -> (x - 1, y + 1),
    \(x, y) -> (x, y - 1),
    \(x, y) -> (x, y + 1),
    \(x, y) -> (x + 1, y - 1),
    \(x, y) -> (x + 1, y),
    \(x, y) -> (x + 1, y + 1)
  ]

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines_ = lines contents
  let grid = makeGrid lines_
  let count_ = countCross grid
  print count_
