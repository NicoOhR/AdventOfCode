import Data.List (intercalate, transpose)
import Data.List.ZigZag (diagonals) -- fucking what
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

main :: IO ()
main = do
  contents <- readFile "test.txt"
  let lines_ = lines contents
  let lineRow = count $ rowWiseLinear lines_
  let lineCol = count $ rowWiseLinear (transpose lines_)
  let linDiagR = count (intercalate [] $ diagonals lines_ :: String)
  let linDiagL = count (intercalate [] $ diagonals (transpose lines_) :: String)
  mapM_ print $ transpose lines_

count :: String -> Int
count x = length $ getAllTextMatches (x =~ regex_ :: AllTextMatches [] String)
  where
    regex_ = "XMAS|SAMX"

-- stupid solution
rowWiseLinear :: [String] -> String
rowWiseLinear xs = loop xs ""
  where
    loop [] s = s
    loop (y : ys) s = loop ys s ++ y
