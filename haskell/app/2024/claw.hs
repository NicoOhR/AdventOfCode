import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Numeric.LinearAlgebra (Matrix, Vector, fromList, fromLists, linearSolve, toLists, tr, (><))
import Text.Regex.TDFA (AllTextMatches, getAllTextMatches, (=~))

solve :: (Matrix Double, [Double]) -> Maybe (Matrix Double)
solve (m, rhs) = linearSolve m b
  where
    b = (2 >< 1) rhs

isInteger :: Double -> Bool
isInteger x = abs (x - fromIntegral (round x)) < 1e-3 -- bullshit magic epsilon WOOYAH

allElems :: Matrix Double -> Bool
allElems m = all (all isInteger) l
  where
    l = toLists m

count :: [Double] -> Int
count [x, y] = 3 * round x + round y

adjust :: [Double] -> [Double]
adjust [x, y] = [x + 1e+13, y + 1e+13]

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines_ = splitOn [""] $ lines contents
  let travesty = map (map (\x -> map (fromIntegral . read) $ getAllTextMatches (x =~ "[0-9]+" :: AllTextMatches [] String) :: [Double])) lines_
  let systems = zip (map (tr . fromLists . take 2) travesty) (map adjust $ concatMap (drop 2) travesty)
  let solutions = mapMaybe solve systems
  let filtered = map (concat . toLists) (filter allElems solutions)
  let total = sum $ map count filtered
  print total
