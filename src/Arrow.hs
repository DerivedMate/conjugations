module Arrow where

import Helpers
import Pattern
import Data.List
import Debug.Trace

data Arrow a = 
  Arrow { i  :: Int
        , j  :: Int
        , el :: a
        } deriving (Eq, Show)

arrowLength :: Arrow a -> Int
arrowLength a = abs $ i a - j a

doArrowsIntersect :: Arrow a -> Arrow b -> Bool
doArrowsIntersect a b = 
  signum (i a - i b) /= signum (j a - j b)
  && arrowLength a   /= arrowLength b

findIntersections :: Arrow a -> [Arrow b] -> [Arrow b]
findIntersections a as = filter (doArrowsIntersect a) as

index :: [a] -> [(a, Int)]
index as = zip as [0..]
arrows0 :: [(Char, Int)] -> [(Char, Int)] -> [Arrow Char]
arrows0 as bs = g as bs []
  where 
    f :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)] 
      -> (Maybe (Arrow Char), [(Char, Int)])
    f x@(a, i) (y@(b, j) : bs) pastBs
      | a == b    = (Just $ Arrow i j a, reverse pastBs ++ bs)
      | otherwise = f x bs (y : pastBs)
    f _ [] pastBs = (Nothing, reverse pastBs)

    g :: [(Char, Int)] -> [(Char, Int)] -> [Arrow Char] -> [Arrow Char]
    g [] _ arrows      = reverse arrows
    g _ [] arrows      = reverse arrows
    g (a:as) bs arrows = 
      case nextArrow of 
        Just arrow -> g as bs' (arrow:arrows)
        _          -> g as bs  arrows
      where (nextArrow, bs') = f a bs []

common :: String -> String -> Pattern
common a b = aux $ arrows0 (index a) (index b)
  where
    aux ars
      | or [doArrowsIntersect a b | a <- ars, b <- ars, a /= b] =
        aux $ filter (doNotIntersectShorter ars) ars
      | otherwise = 
        foldArrows ars 0 a (Pattern "" [] "")
      where 
        doNotIntersectShorter ars a = 
          all 
            ((>= arrowLength a) . arrowLength)  
            $ findIntersections a ars
        foldArrows :: [Arrow Char] -> Int -> String -> Pattern
                   -> Pattern
        foldArrows [] _ cs (Pattern commonPart ds suffix) =
          Pattern 
            (reverse commonPart) 
            (reverse ds) 
            (suffix <> cs)
        foldArrows ast@(a:as) n (c:cs) (Pattern commonPart ds suffix)
          | i a == n  = foldArrows 
            as (n + 1) cs 
            (Pattern (c : commonPart) ds suffix)
          | otherwise = foldArrows 
            ast (n + 1) cs 
            (Pattern commonPart ((n-1, c) : ds) suffix)