import Data.List (sort)
import Data.Map (member)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

wordsToTuple [x, y] = (x, y)

tupleDifference (x, y) = abs (x - y)

addToFreq :: Int -> Map.Map Int Int -> Map.Map Int Int
addToFreq x = Map.insertWith (+) x 1

freqMap = foldl (flip addToFreq) Map.empty

joinMaps :: Map.Map Int Int -> Map.Map Int Int -> Map.Map Int Int
joinMaps first_map second_map =
  Map.foldlWithKey
    ( \acc key value_one ->
        case Map.lookup key second_map of
          Just value_two -> Map.insert key (value_one * value_two) acc
          Nothing -> Map.insert key 0 acc
    )
    Map.empty
    first_map

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines_of_file = lines contents
  let (first, second) = unzip $ map (wordsToTuple . words) lines_of_file
  let (first_num, second_num) = (sort $ map read first :: [Int], sort $ map read second :: [Int])
  let (freq_one, freq_two) = (freqMap first_num, freqMap second_num)
  let (numbers, freq) = unzip $ Map.toList $ joinMaps freq_one freq_two
  let total = sum $ zipWith (*) numbers freq
  print total
