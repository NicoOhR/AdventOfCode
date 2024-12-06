import Data.List.Split (splitOn, splitWhen)

type Rule = (Int, Int)

type Update = [Int]

-- plan
-- without worrying about DP, we can look over every element
-- of the Update, and check if it is an element of either side of the Rule
-- Then check against the rule itselfs, once one failes, we go to the next
-- We append all passing to a list, and sum the middle element

main :: IO ()
main = do
  file_ <- readFile "input.txt"
  let (rules', updates') = (\[x, y] -> (x, y)) $ splitWhen (== "") $ lines file_ -- ghc can cry abt it
  let rules = map readToRule rules'
  let updates = map readToUpdate updates'
  let (correct, _) = unzip $ filter snd (zip updates (map (`adheres` rules) updates))
  let total = sum $ map middle correct
  print total

middle xs = xs !! (length xs `div` 2)

readToRule :: String -> Rule
readToRule s = (\[x, y] -> (x, y)) $ map read $ splitOn "|" s

readToUpdate :: String -> Update
readToUpdate s = map read $ splitOn "," s

checkAgainstRule :: Update -> Rule -> Bool
checkAgainstRule xs (first, second) =
  loop xs
  where
    loop [] = True
    loop (element : update)
      | element == second && first `elem` update = False
      | otherwise = loop update

adheres :: Update -> [Rule] -> Bool
adheres [] _ = True
adheres x rs = loop x rs
  where
    loop _ [] = True
    loop update (rule : rules)
      | checkAgainstRule update rule = loop update rules
      | otherwise = False
