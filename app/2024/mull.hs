import Data.List.Split
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

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

main :: IO ()
main = do
  file_ <- readFile "input.txt"
  let regex_ = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"
      matches = getAllTextMatches (file_ =~ regex_ :: AllTextMatches [] String)
      products = map multiply matches
      total = sum products
  print total
