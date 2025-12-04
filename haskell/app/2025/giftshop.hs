import Data.List.Split (splitOn)

digs :: (Integral x) => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

isInvalid :: Int -> Bool
isInvalid x
    | odd (length $ show x) =
        False
    | leftList == rightList =
        True
    | otherwise = False
  where
    (leftList, rightList) = splitAt (length xs `div` 2) xs
    xs = digs x

findInvalid :: (Int, Int) -> [Int]
findInvalid (x, y) = fst <$> filter snd (fmap (\n -> (n, isInvalid n)) [x .. y])

main :: IO ()
main = do
    content <- readFile "input.txt"
    let split = (\[x, y] -> (read x :: Int, read y :: Int)) . splitOn "-" <$> splitOn "," content
    let test = sum $ concatMap findInvalid split
    print test
