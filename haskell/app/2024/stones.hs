getDigits :: Int -> [Int]
getDigits n = map (read . (: [])) (show (abs n))

splitStone :: Int -> (Int, Int)
splitStone n = (fromDigits leftDigits, fromDigits rightDigits)
  where
    digits = getDigits n
    half = length digits `div` 2
    (leftDigits, rightDigits) = splitAt half digits

    fromDigits :: [Int] -> Int
    fromDigits = foldl (\acc d -> acc * 10 + d) 0

stones :: [Int] -> [Int]
stones s = loop s []
  where
    loop :: [Int] -> [Int] -> [Int]
    loop [] a = a
    loop (x : xs) a
      | x == 0 = loop xs (a ++ [1])
      | even $ length $ getDigits x = loop xs (a ++ [left, right])
      | otherwise = loop xs (a ++ [x * 2024])
      where
        (left, right) = splitStone x

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let stones_ = map read $ words contents :: [Int]
  let iteration = length (iterate stones stones_ !! 25)
  print iteration
