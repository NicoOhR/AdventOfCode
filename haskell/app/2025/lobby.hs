import Data.Char (digitToInt, intToDigit)
import Data.Sequence (Seq (Empty), empty, fromList, viewl, viewr)
import Debug.Trace (traceShowId)

main :: IO ()
main = do
    content <- lines <$> readFile "input.txt"
    let integers = map (map digitToInt) $ init content
    let p1 = map partOne integers
    let p2 = map partTwo integers
    print p2

partTwo :: [Int] -> Seq Int
partTwo list = go list empty :: Seq Int
  where
    go :: [Int] -> Seq Int -> Seq Int
    go [] q = q
    go (x : xs) q

partOne :: [Int] -> Int
partOne list = go list 0 0
  where
    go :: [Int] -> Int -> Int -> Int
    go [] f s = f * 10 + s
    go [x] f s
        | x > s = f * 10 + x
        | otherwise = f * 10 + s
    go (x : xs) f s
        | x > f = go xs x 0
        | x > s = go xs f x
        | otherwise = go xs f s
