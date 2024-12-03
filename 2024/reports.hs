smooth :: (Num a, Ord a) => [a] -> Bool
smooth [] = True
smooth [_] = True
smooth xs = all (\d -> abs d <= 3 && abs d >= 1) difference
  where
    difference = zipWith (-) (tail xs) xs

monotone :: (Num a, Ord a) => [a] -> Bool
monotone [] = True
monotone [_] = True
monotone xs = all (\d -> signum d == sign) difference
  where
    difference = zipWith (-) (tail xs) xs
    sign = signum $ head difference

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines_ :: [[Int]] = map (map read . words) (lines contents)
  let both = zipWith (&&) (map monotone lines_) (map smooth lines_)
  let count = length $ filter id both
  print count
