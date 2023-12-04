import System.IO ()
import Data.Char (isDigit)
import Data.List (isPrefixOf)



isSpecial :: Char -> Bool
isSpecial x | x == '*' = True | x == '=' = True | x == '/' = True | x == '$' = True | x == '%' = True | x == '@' = True | otherwise = False

group :: String -> [String]
group [] = []
group (x:xs) 
        | isDigit x = takeWhile isDigit(x:xs) : group (dropWhile isDigit xs)
        | otherwise = group xs

group' :: String -> [String]
group' [] = []
group' (x:xs) 
        | isSpecial x = takeWhile isSpecial(x:xs) : group' (dropWhile isSpecial xs)
        | otherwise = group' xs

matchIndex :: String -> String -> Int -> (String,Int)
matchIndex x s n | x `isPrefixOf` s = (x, n) | null s = ("", -1) | otherwise = matchIndex x (tail s) (n + 1) 

matchIndices :: String -> String -> Int -> [Int]
matchIndices x s n
  | null s           = []
  | x `isPrefixOf` s = n : matchIndices x (drop (length x) s) (n + length x)
  | otherwise        = matchIndices x (tail s) (n + 1)

applyMatchIndices :: [[String]] -> [String] -> [(String, [Int])]
applyMatchIndices listOfLists listOfStrings =
    [ (x, matchIndices x s 0) | s <- listOfStrings, lst <- listOfLists, x <- lst, not (null (matchIndices x s 0)) ]

applyMatchIndex :: [[String]] -> [String] -> [(String, Int)]
applyMatchIndex listOfLists listOfStrings = [ matchIndex x s 0 | s <- listOfStrings, lst <- listOfLists, x <- lst ]

cleanMatchIndex :: [(String, Int)] -> [(String, Int)]
cleanMatchIndex = filter (/= ("",-1))



main :: IO()
main = do
    content <- readFile "input.txt"
    --generate the integar lists
    let linesOfFile = lines content
    let groupedLines = map group linesOfFile
    let indexedLines = cleanMatchIndex $ applyMatchIndex groupedLines linesOfFile
    --generate the special lists
    let groupedSpecial = map group' linesOfFile
    let groupedSpecialClean = applyMatchIndices groupedSpecial linesOfFile

    mapM_ print indexedLines
    mapM_ print groupedSpecialClean