import Data.Char (digitToInt)

checksum :: String -> Int
checksum s = loop s 0 0
  where
    loop [] accum _ = accum
    loop (x : xs) accum i
      | x == '.' = loop xs accum (i + 1)
      | otherwise = loop xs (accum + i * digitToInt x) (i + 1)

defrag :: String -> String
defrag s = loop s (reverse s) []
  where
    n = length $ filter (/= '.') s
    k = length s - n
    loop [] [] f = take n f
    loop [] _ f = take n f
    loop _ [] f = take n f
    loop (x : xs) (y : ys) f
      | x /= '.' =
          loop xs (y : ys) (f ++ [x])
      | otherwise = case y of
          '.' -> loop (x : xs) ys f
          _ -> loop xs ys (f ++ [y])

ir :: String -> String
ir s = loop s [] 0
  where
    loop :: String -> String -> Int -> String
    loop [] acc _ = acc
    loop (x : y : xs) acc i =
      let rep1 = replicate (digitToInt x) (head $ show i)
          rep2 = replicate (digitToInt y) '.'
       in loop xs (acc ++ rep1 ++ rep2) (i + 1)
    loop [y] acc i =
      let final = replicate (digitToInt y) (head $ show i)
       in loop [] (acc ++ final) (i + 1)

main :: IO ()
main = do
  line_ <- readFile "input.txt"
  let intermediate = defrag $ ir $ filter (/= '\n') line_
  print intermediate
