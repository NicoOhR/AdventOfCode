import System.IO
import Data.List.Split

clean :: [Char] -> [Char]
clean (x:xs) | x == ':' = xs | otherwise = clean $ tail xs

group :: String -> [String]
group = splitOn " "

split' :: String -> [String] -> ([String],[String])
split' d (x:xs) | x == d = ([],xs) | otherwise = let (before, after) = split' d xs in (x: before, after )

total :: Int -> ([String],[String])  -> Int
total n ([], ys)  = n
total n (x:xs, ys)  | x `elem` ys = total (n+1) (xs, ys)  | otherwise = total n (xs, ys) 

score :: Int -> Int
score n | n == 0 = 0 | otherwise =  2^(n-1)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let linesOfFile = lines content
    let cleaned = map clean linesOfFile
    let splited = map group cleaned
    let filtered = map (filter (/= "")) splited
    let splited' = map (split' "|") filtered
    let totals = map (total 0) splited'
    let scores = map score totals
    let solution = sum scores
    print solution
