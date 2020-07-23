module Helpers where

import Data.List

(<&>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<&>) a b c = a c && b c

(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<||>) a b c = a c || b c

stringOfChar :: Char -> String
stringOfChar c = [c]

normalizePath :: String -> String
normalizePath dist
  | endWithDash = normalizePath $ init dist
  | otherwise   = dist
  where 
    endWithDash = (last dist) == '/'

insertAt :: Ord a => a -> Int -> [a] -> [a]
insertAt x i ys = 
  map snd
  $ insertBy aux (i, x) 
  $ zip [0..] ys 
  where 
    -- the default overload of `insert` inserts when `LT || EQ`, which is ambiguous. This function overloads iff `LT` 
    aux (i, _) (j, _) 
      | i < j     = LT
      | otherwise = GT

{- 
  addresses an error, as perceived, of `intersect` returning `"fallecer" `intersect` "fallezco" == "fallece"`; 
  instead, returns "fallec", for "fallezco" only contains one 'e', whereas the result contains two.
-}
intersect' :: Ord a => [a] -> [a] -> [a]
intersect' a b = aux [] a b
  where 
    aux acc a b
      | isExhausted = reverse acc
      | el `elem` b = aux (el : acc) (tail a) (delete el b)
      | otherwise   = aux acc (tail a) b
      where 
        el          = head a
        isExhausted = length    a == 0
                      || length b == 0

unpackMaybeList :: Maybe [a] -> [a]
unpackMaybeList Nothing  = []
unpackMaybeList (Just a) = a

-- Groups an array into sub-arrays of given lengths
partitionToLengths :: Ord a => [Int] -> [a] -> [[a]]
partitionToLengths ls xs = aux [] xs ls
  where 
    aux acc [] _      = reverse acc
    aux acc _ []      = reverse acc
    aux acc xs (l:ls) = aux ((take l xs) : acc) (drop l xs) ls

groupByLengths :: Foldable t => [t a] -> [[t a]]
groupByLengths xs = groupBy hasEqLen xs
  where hasEqLen a b  = length a == length b