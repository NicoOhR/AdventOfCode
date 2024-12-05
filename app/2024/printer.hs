import Data.List.Split (splitOn, splitWhen)

type Rule = (Int, Int)

type Update = [Int]

readToRule :: String -> Rule
readToRule s = (\[x, y] -> (x, y)) $ map read $ splitOn "|" s

readToUpdate :: String -> Update
readToUpdate s = map read $ splitOn "," s

main :: IO ()
main = do
  file_ <- readFile "test.txt"
  let (rules', updates') = (\[x, y] -> (x, y)) $ splitWhen (== "") $ lines file_ -- ghc can cry abt it
  let rules = map readToRule rules'
  let updates = map readToUpdate updates'
  mapM_ print updates
