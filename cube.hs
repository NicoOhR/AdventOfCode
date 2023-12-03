import System.IO ()
import Data.Char (isAlphaNum)
import Text.Read (readMaybe)
--impossible :: [String] -> Bool

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"

possible :: [String] -> Bool
possible [number, color] = case readMaybe number of 
                                    Just num -> case color of
                                         "green" -> num <= 13
                                         "blue"  -> num <= 14
                                         "red"   -> num <= 12
                                         _       -> False

removeFirst :: [[String]] -> [[String]]
removeFirst (_:xs) = xs

possibleGame :: [[String]] -> [Bool]
possibleGame = map possible

possibleGame' :: [Bool] -> Bool
possibleGame' = and

trueIndex :: [Bool] -> [Int]
trueIndex bools = [idx | (val, idx) <- zip bools [0..], val]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let linesOfFile = lines content
    let splitLines = map words linesOfFile
    let sanitizedLines = map (map (filter isAlphaNum)) splitLines
    let groupedLine = map (group 2)  sanitizedLines
    let finalLine = map removeFirst groupedLine
    let trueLine = map (possibleGame' . possibleGame) finalLine
    let finalValue = trueIndex trueLine
    let finalValue' = map (+1) finalValue
    let total = sum finalValue'
    print total 