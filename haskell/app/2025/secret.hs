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

main :: IO ()
main = do
    content <- readFile "input.txt"
    let linesOfFile = lines content
    let zeros = countZeros $ getPositions $ map makeInteger linesOfFile
    print zeros
