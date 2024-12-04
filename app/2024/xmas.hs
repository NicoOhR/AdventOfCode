import Data.List (intercalate, transpose)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

type Point = (Int, Int)

makeGrid :: [String] -> Map.Map Point Char
makeGrid rows =
  Map.fromList [((r, c), char) | (r, row) <- zip [0 ..] rows, (c, char) <- zip [0 ..] row]

explore :: Point -> Map.Map Point Char -> Bool
explore p m =
  case Map.lookup p m of
    Just 'X' -> findXmas p m
    _ -> False

findXmas :: Point -> Map.Map Point Char -> Bool
findXmas start m = loop start "XMAS"
  where
    loop _ [] = True
    loop p (c : cs) =
      case Map.lookup p m of
        Just char
          | char == c ->
              trace (show p) $
                any (`loop` cs) (neighbors p)
        _ -> False
    neighbors :: Point -> [Point]
    neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1)]

exploreGrid :: Map.Map Point Char -> [Point]
exploreGrid m = filter (`explore` m) (Map.keys m)

main :: IO ()
main = do
  contents <- readFile "test.txt"
  let lines_ = lines contents
  let grid = makeGrid lines_
  let matches = exploreGrid grid
  mapM_ print (matches)
