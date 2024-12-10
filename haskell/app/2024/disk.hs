import Data.Char (digitToInt)
import qualified Data.IntMap as M
import qualified Data.IntSet as S

type Disk = M.IntMap Int

type Free = S.IntSet

main :: IO ()
main = do
  contents <- readFile "test.txt"
  let diskMap = fmap digitToInt contents
  mapM_ print diskMap
