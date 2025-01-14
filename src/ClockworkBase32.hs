module ClockworkBase32
  ( maxInList
  ) where

-- returns the maximum element in a list, if it exists
maxInList :: (Ord a) => [a] -> Maybe a
maxInList [] = Nothing
maxInList xs = Just (maximum xs)
