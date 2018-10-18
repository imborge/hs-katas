module Day1 where

-- |Should return the (first found) integer index of the target in the array,
-- or -1 of the target is not in the array.
chop :: Int -> [Int] -> Int
chop target xs = go target xs 0
  where
    go _      [] _          = (-1)
    go target xs startIndex =
          case compare target (xs !! midIndex) of
            EQ -> startIndex + midIndex
            LT -> go target (take midIndex xs) startIndex
            GT -> go target (drop (midIndex+1) xs) (startIndex + midIndex + 1)
          where midIndex = length xs `div` 2
