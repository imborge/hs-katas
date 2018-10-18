import Test.QuickCheck

import Day1
import Data.List

prop_chop :: Int -> [Int] -> Bool
prop_chop x xs =
  case chop x ys of
    (-1) -> x `elemIndices` ys == []
    n    -> n `elem` (x `elemIndices` ys)
  where ys = sort xs

main :: IO ()
main = do
  quickCheck prop_chop
