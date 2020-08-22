{-# LANGUAGE OverloadedStrings #-}


module Formal where

import Helpers
import Data.Aeson

data Formal l2T l1T l0T = 
  Formal { l2 ::  l2T  -- Ful conjugation
         , l1 :: [l1T] -- Moods
         , l0 :: [l0T] -- Tenses
         } deriving (Show, Eq)

type FormalS  a     = Formal [[[a]]] [[a]] [a]
type FormalST a b c = Formal [[[a]]] [[b]] [c]

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (Formal a b c) where
  toJSON f = object 
    [ "l2" .= l2 f
    , "l1" .= l1 f
    , "l0" .= l0 f
    ]
  toEncoding f = pairs 
    $  "l2" .= l2 f
    <> "l1" .= l1 f
    <> "l0" .= l0 f

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (Formal a b c)  where
  parseJSON (Object v) = Formal 
    <$> v .: "l2"
    <*> v .: "l1"
    <*> v .: "l0"

  parseJSON _          = mempty

formalCompare :: (Eq a, Eq b, Eq c) => 
  FormalST a b c -> 
  FormalST a b c -> 
  Formal Bool Bool Bool
formalCompare a b = 
  Formal 
    (l2 a == l2 b)
    (zipWith (==) (l1 a) (l1 b))
    (zipWith (==) (l0 a) (l0 b))

makeFormal :: [[[a]]] -> Formal [[[a]]] [[a]] [a]
makeFormal a = Formal 
  a
  b
  (concat b)
  where 
    b = map wrap $ concat a
    wrap a = [a]
  
formalMap :: (  a2 ->   b2)
          -> (L a1 -> L b1)
          -> (L a0 -> L b0)
          -> Formal a2 a1 a0
          -> Formal b2 b1 b0
formalMap f g h a = Formal
  (f $ l2 a)
  (g $ l1 a)
  (h $ l0 a)



formalZipWith :: (  a2 ->   b2 ->   c2) 
              -> (L a1 -> L b1 -> L c1) 
              -> (L a0 -> L b0 -> L c0)
              -> Formal a2 a1 a0
              -> Formal b2 b1 b0
              -> Formal c2 c1 c0
formalZipWith f g h a b = Formal
  (f (l2 a) (l2 b))
  (g (l1 a) (l1 b))
  (h (l0 a) (l0 b))