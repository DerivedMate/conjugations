{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import Data.List
import Control.Applicative (empty)
import Control.Monad
import System.IO
import Debug.Trace
import Data.Aeson

type L  a =   [a]
type L2 a =  [[a]]
type L3 a = [[[a]]]

data OrWriter a = OrWriter Bool a
readOr :: OrWriter a -> Bool
readOr (OrWriter r _) = r

deOr :: OrWriter a -> a
deOr (OrWriter _ a) = a

instance Functor OrWriter where
  fmap f (OrWriter r a) = OrWriter r (f a)

instance Applicative OrWriter where
  pure a = OrWriter False a
  (<*>) (OrWriter r f) (OrWriter r' a) = 
    OrWriter (r || r') (f a)

instance Monad OrWriter where
  (>>=) (OrWriter r a) f = OrWriter (r || r') b
    where (OrWriter r' b) = f a

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
    endWithDash = last dist == '/'

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
        isExhausted = null a || null b

-- data ArrowT a = ArrowT a Int Int

unpackMaybeList :: Maybe [a] -> [a]
unpackMaybeList Nothing  = []
unpackMaybeList (Just a) = a

-- Groups an array into sub-arrays of given lengths
partitionToLengths :: Ord a => [Int] -> [a] -> [[a]]
partitionToLengths ls xs = aux [] xs ls
  where 
    aux acc [] _      = reverse acc
    aux acc _  []     = reverse acc
    aux acc xs (l:ls) = aux (take l xs : acc) (drop l xs) ls

groupByLengths :: Foldable t => [t a] -> [[t a]]
groupByLengths xs = groupBy hasEqLen xs
  where hasEqLen a b  = length a == length b

readLines :: FilePath -> IO [String]
readLines file =
  openFile file ReadMode
  >>= hGetContents
  >>= pure . lines


dropWhileWhole :: Ord a => ([a] -> Bool) -> [a] -> [a]
dropWhileWhole f (x:xs) 
  | f xs      = dropWhileWhole f xs
  | otherwise = xs

-- TODO: ¿replace with `takeWhile (not . (== a)) as`?
splitAtEl :: Ord a => a -> [a] -> [a]
splitAtEl a as = fst $ break (== a) as

wrap :: a -> [a]
wrap a = [a]

verbalize :: FilePath -> String
verbalize path = takeWhile (/= '.') 
                 $ dropWhileWhole (elem '/') path

cmpList :: Eq a => [a] -> [a] -> [Bool]
cmpList a b = zipWith (==) a b

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs