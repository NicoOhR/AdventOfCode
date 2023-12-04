import System.IO
import Data.Char (isDigit)
import Data.List (isPrefixOf)

group :: String -> [String]
group [] = []
group (x:xs) 
        | isDigit x = takeWhile isDigit(x:xs) : group (dropWhile isDigit xs)
        | otherwise = group xs

matchIndex :: String -> String -> Int -> (String,Int)
matchIndex x s n | x `isPrefixOf` s = (x, n) | otherwise = matchIndex x (tail s) (n + 1)

main :: IO()
main = do
    content <- readFile "input.txt"
    let linesOfFile = lines content
    let groupedLines = map group linesOfFile
    mapM_ print groupedLines