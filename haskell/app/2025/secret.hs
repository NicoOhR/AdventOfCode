import Debug.Trace

makeInteger :: String -> Integer
makeInteger [] = 0
makeInteger (x : xs)
    | x == 'L' = -read xs
    | x == 'R' = read xs
    | otherwise = 0

getPositions :: [Integer] -> [Integer]
getPositions = scanl (\x y -> (x + y) `mod` 100) 50

countZeros :: [Integer] -> Int
countZeros = length . filter (== 0)

countSwitches :: Integer -> Integer -> Integer
countSwitches currentPosition turn
    | newPos <= 0 = abs ((newPos) `div` 100) - if currentPosition == 0 then 1 else 0
    | otherwise = newPos `div` 100
  where
    newPos = currentPosition + turn

totalSwitches :: [Integer] -> Integer
totalSwitches = snd . foldl step (50, 0)
  where
    step :: (Integer, Integer) -> Integer -> (Integer, Integer)
    step (cur, acc) v =
        let next = (cur + v) `mod` 100
         in (next, acc + countSwitches cur v)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let linesOfFile = lines content
    let zeros = countZeros $ getPositions $ map makeInteger linesOfFile
    let switches = totalSwitches $ map makeInteger linesOfFile
    print zeros
    print switches
