{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import Data.List
import Control.Applicative (empty)
import Control.Monad
import System.IO
import Debug.Trace
import Data.Aeson

type Arrow      = (Int, Int)
type Embedded a = [[[a]]]
data Indexed a  = Indexed Int a
  deriving (Eq)

instance (Show a) => Show (Indexed a) where
  show (Indexed c a) = "{\"count\":" <> show c <> ", \"item\":" <> show a <> "}\n\n"

type L a = [a]
type L2 a = [[a]]
type L3 a = [[[a]]]

(<&>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<&>) a b c = a c && b c

(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<||>) a b c = a c || b c

stringOfChar :: Char -> String
stringOfChar c = [c]

boolOfMaybe :: Ord a => Maybe a -> Bool
boolOfMaybe (Just _) = True
boolOfMaybe Nothing  = False

normalizePath :: String -> String
normalizePath dist
  | endWithDash = normalizePath $ init dist
  | otherwise   = dist
  where 
    endWithDash = (last dist) == '/'

{-
  Inserts the given item right after the given index.
  If the index is greater than the total length, the item is inserted at the end.
  On the other hand, if it's less than 0, it is inserted at the beginning.
-}
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
  Deprecated, for it fails when one of the sets still contains duplicates of a certain common element after the other set is already exhausted. Example: `intersect' "fuere" "fuéremos"`
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

lengthOfArrow :: Arrow -> Int
lengthOfArrow (a, b) = abs $ a - b

doLinesIntersect :: Arrow -> Arrow -> Bool
doLinesIntersect a@(i, j) b@(k, l) =
     signum (i - k)  /= signum (j - l)
  && lengthOfArrow a /= lengthOfArrow b

reduceArrows :: [Arrow] -> [Arrow]
reduceArrows as = aux [] as
  where
    finder (i, j) (k, l) = i == k || j == l
    aux past []     = reverse past
    aux past (a:as)
      | wasAlready = aux past as
      | otherwise  = aux (a:past) as
      where
        wasAlready = boolOfMaybe $ find (finder a) past

-- Ignores an intersection if the arrows are of the same length
findIntersections :: Arrow -> [Arrow] -> [Arrow]
findIntersections a as = filter (doLinesIntersect a) as

isStraight :: Arrow -> Bool
isStraight (a, b) = a == b

common :: String -> String -> String
common a b = aux aa0
  where 
    aa0        = (ars0 aInd bInd)
    aInd       = ind a
    bInd       = ind b
    ars0 as bs = [ (i, j) 
                 | (a, i) <- as
                 , (b, j) <- bs
                 , a == b
                 ]
    ind a      = zip a [0..]
    deInd a    = fst $ unzip a
    aux ars 
      | any (uncurry doLinesIntersect) [(a, b) | a <- ars, b <- ars] = 
        aux $ filter (f ars) ars
      | otherwise = 
        map ((a !!) . fst) $ reduceArrows ars
    f ars a = all (>= len) intersectLengths
      where 
        intersectLengths = map lengthOfArrow intersects
        intersects = findIntersections a ars
        len = lengthOfArrow a

-- data ArrowT a = ArrowT a Int Int

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

readLines :: FilePath -> IO [String]
readLines file =
  openFile file ReadMode
  >>= hGetContents
  >>= pure . lines

-- longest :: (Ord a, Foldable t) => t a -> a
longest xs = maximumBy aux xs
  where aux a b = compare (length a) (length b)

dropWhileWhole :: Ord a => ([a] -> Bool) -> [a] -> [a]
dropWhileWhole f (x:xs) 
  | f xs      = dropWhileWhole f xs
  | otherwise = xs

splitAtEl :: Ord a => a -> [a] -> [a]
splitAtEl a as = fst $ break (== a) as

formalize :: L3 a -> L (L3 a)
formalize as = concat [as : bs, cs]
  where 
    cs = map wrap $ concat $ map (map wrap) as
    bs = map wrap as
    wrap a = [a]