import System.IO 
import Data.Char (isDigit)

filterNumb :: String -> String
filterNumb = filter isDigit 

realNumber :: String -> String
realNumber [x] = [x,x]
realNumber xs = [head xs, last xs] 

strToNumb :: [String] -> [Int]
strToNumb = map read . filter (all isDigit)


main = do
    content <- readFile "input.txt"
    let linesOfFile = lines content
    let strings = map (realNumber . filterNumb) linesOfFile
    let numbers = strToNumb strings
    let total = sum numbers
    print total