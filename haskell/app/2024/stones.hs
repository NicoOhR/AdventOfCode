blink :: Int -> [Int]
blink 0 = [1]
blink n
  | even length_ = map read [take k s, drop k s]
  | otherwise = [n * 2024]
  where
    s = show n
    length_ = length s
    k = length_ `div` 2

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let stones_ = map read $ words contents :: [Int]
  let iteration = length $ iterate (concatMap blink) stones_ !! 25
  print iteration
