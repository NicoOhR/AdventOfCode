import System.IO ()
import Data.Char (isAlphaNum)
import Text.Read (readMaybe)
--impossible :: [String] -> Bool

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"

possible :: [String] -> Bool
possible [number, color] = case readMaybe number of 
                                    Just num -> case color of
                                         "green" -> num < 13
                                         "blue"  -> num < 14
                                         "red"   -> num < 11
                                         _       -> False


main :: IO ()
main = do
    content <- readFile "input.txt"
    let linesOfFile = lines content
    let splitLines = map words linesOfFile
    let sanitizedLines = map (map (filter isAlphaNum)) splitLines
    let finalLine = map (group 2)  sanitizedLines
    mapM_ print finalLine 