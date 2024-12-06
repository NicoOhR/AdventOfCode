import Data.List (permutations, sortBy)
import Data.List.Split (splitOn, splitWhen)
import Text.Printf (printf)

type Rule = (Int, Int)

type Update = [Int]

-- Part 1
-- without worrying about DP, we can look over every element
-- of the Update, and check if it is an element of either side of the Rule
-- Then check against the rule itselfs, once one failes, we go to the next
-- We append all passing to a list, and sum the middle element
--
-- Part 2
-- since I already have the mechaniations for if something is correct or not
-- and I also have several upcoming and very pressing exams, it's generally
-- easier to just judge all permutations, and pick out the one that works
-- It won't get me a job at Jane Street but if you can forgive I can forget
--
-- Part 2 Part 2
-- Since the above turns out to be a O(exp(n!)) algorithm, we'll try to be a bit
-- more correct by creating an ordering from the rules and sorting that

main :: IO ()
main = do
  file_ <- readFile "input.txt"
  let (rules', updates') = (\[x, y] -> (x, y)) $ splitWhen (== "") $ lines file_ -- ghc can cry abt it
  let rules = map readToRule rules'
  let updates = map readToUpdate updates'
  let (correct, _) = unzip $ filter snd (zip updates (map (`adheres` rules) updates))
  let (incorrect, _) = unzip $ filter (\(_, b) -> not b) (zip updates (map (`adheres` rules) updates))
  let fixed = map (sortBy (rulesOrdering rules)) incorrect
  let p1 = sum $ map middle correct
  let p2 = sum $ map middle fixed
  print p2

middle xs = xs !! (length xs `div` 2)

rulesOrdering :: [Rule] -> Int -> Int -> Ordering
rulesOrdering rules a b
  | a == b = EQ
  | (a, b) `elem` rules = LT
  | (b, a) `elem` rules = GT
  | otherwise = error (printf "Unknown case")

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
