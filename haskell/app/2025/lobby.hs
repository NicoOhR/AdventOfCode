import Data.Char (digitToInt)

main :: IO ()
main = do
    content <- lines <$> readFile "input.txt"
    let integers = map (map digitToInt) $ init content
    let biggest = map partOne integers
    print $ sum biggest

partOne :: [Int] -> Int
partOne list = go list 0 0
  where
    go :: [Int] -> Int -> Int -> Int
    go [] f s = f * 10 + s
    go [x] f s
        | x > s = f * 10 + x
        | otherwise = f * 10 + s
    go (x : xs) f s
        | x > f = go xs x 0
        | x > s = go xs f x
        | otherwise = go xs f s
