import Data.List
import Debug.Trace (trace)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines_ :: [[Int]] = map (map read . words) (lines contents)
  let both = zipWith (&&) (map monotone lines_) (map smooth lines_)
  let dampened = length $ filter id (map dampener lines_)
  let count = length $ filter id both
  print dampened

smooth :: (Num a, Ord a) => [a] -> Bool
smooth xs = all (\d -> abs d <= 3 && abs d >= 1) difference
  where
    difference = zipWith (-) (tail xs) xs

monotone :: (Num a, Ord a) => [a] -> Bool
monotone xs = all (\d -> signum d == sign) difference
  where
    difference = zipWith (-) (tail xs) xs
    sign = signum $ head difference

dampener :: [Int] -> Bool
dampener xs = any (\p -> smooth p && monotone p) possible
  where
    possible = zipWith (++) ns ts'
    ns = inits xs
    ts = tails xs
    ts' = map tail (init ts)

-- not working
removeBasedDampner :: (Num a, Ord a, Show a) => [a] -> [a]
removeBasedDampner [] = []
removeBasedDampner [a] = [a]
removeBasedDampner xs = case unsafeLevel of
  Just i -> take (i + 1) xs ++ drop (i + 2) xs
  Nothing -> xs
  where
    difference = zipWith (-) (tail xs) xs
    sign = signum $ head difference
    unsafeLevel = findIndex (\d -> not (abs d <= 3 && abs d >= 1) || signum d /= sign) difference
