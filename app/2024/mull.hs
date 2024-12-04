import Data.List.Split
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Data.List (findIndices)
import Debug.Trace 

main :: IO ()
main = do
  file_ <- readFile "input.txt"
  let regex_ = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|do\\(\\)|don'?t\\(\\)"
      matches = getAllTextMatches (file_ =~ regex_ :: AllTextMatches [] String)
      --products = map multiply matches
      --total = sum products
      total = remove matches
  print total


remove :: [String] -> Int
remove xs = loop xs 0 True
  where 
    loop [] acc _ = acc
    loop (y:ys) acc accum 
      | y == "don't()" = loop ys acc False
      | y == "do()" = loop ys acc True
      | accum = loop ys (acc + multiply y) accum
      | otherwise = loop ys acc accum


process :: [String] -> [String]
process [first, second] = [tail first, init second]
process _ = []

multiply :: String -> Int
multiply input = x * y
  where
    numbers = drop 3 input
    list = process $ splitOn "," numbers :: [String]
    x = read $ head list :: Int
    y = read $ head (tail list) :: Int


