module Util.List where

dedup :: Eq a => [a] -> [a]
dedup []     = []
dedup (x:[]) = [x]
dedup (x1:x2:xs)
  | x1 == x2  = dedup (x1:xs)
  | otherwise = x1 : dedup (x2:xs)
