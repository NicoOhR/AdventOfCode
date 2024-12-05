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
  file_ <- readFile "test.txt"
  let (rules', updates') = (\[x, y] -> (x, y)) $ splitWhen (== "") $ lines file_ -- ghc can cry abt it
  let rules = map readToRule rules'
  let updates = map readToUpdate updates'
  mapM_ print updates

readToRule :: String -> Rule
readToRule s = (\[x, y] -> (x, y)) $ map read $ splitOn "|" s

readToUpdate :: String -> Update
readToUpdate s = map read $ splitOn "," s
