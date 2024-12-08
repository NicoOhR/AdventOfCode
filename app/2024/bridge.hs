{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
import Data.Char (isDigit)

possible :: [Int] -> Bool
possible (y : ys) = loop y ys accum
  where
    accum = 0
    loop target (x : xs) a = loop target xs (x * a) || loop target xs (x + a)
    loop target [] a
      | target == a = True
      | target /= a = False

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines_ = lines contents
  let words_ = map words lines_
  let lists = map (map read) (zipWith ((:)) (map (init . head) words_) (map tail words_)) :: [[Int]]
  let valid = filter fst $ zip (map possible lists) lists
  let total = sum $ map (head . snd) valid
  print total
