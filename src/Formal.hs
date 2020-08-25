{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}


module Formal where

import Helpers
import Data.Aeson

data Idd a = Idd Int a
  deriving (Show)

instance Semigroup a => Semigroup (Idd a) where
  (<>) (Idd id a) (Idd _ b) = Idd id (a <> b)

instance ToJSON a => ToJSON (Idd a) where  
  toJSON (Idd id a) = object
    [ "id"   .= id
    , "data" .= a
    ]

  toEncoding (Idd id a) = pairs 
    $  "id"   .= id
    <> "data" .= a

identify :: [a] -> [Idd a]
identify as = zipWith aux [0..] as
  where aux id a = Idd id a

instance Eq a => Eq (Idd a) where
  (==) (Idd ida a) (Idd idb b) = ida == idb && a == b

data Formal l2T l1T l0T = 
  Formal { l2 :: [l2T] -- Ful conjugations
         , l1 :: [l1T] -- Moods
         , l0 :: [l0T] -- Tenses
         } deriving (Show, Eq)

type FormalS  a     = Formal [[[a]]] [[a]] [a]
type FormalST a b c = Formal [[[a]]] [[b]] [c]
type FormalSS a     = Formal a a a

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
    (l2 a `cmpList` l2 b)
    (l1 a `cmpList` l1 b)
    (l0 a `cmpList` l0 b)

makeFormal :: [[[a]]] -> Formal [[[a]]] [[a]] [a]
makeFormal a = Formal [a] b (concat b)
  where 
    b = map wrap $ concat a
  
formalMap :: (L a2 -> L b2)
          -> (L a1 -> L b1)
          -> (L a0 -> L b0)
          -> Formal a2 a1 a0
          -> Formal b2 b1 b0
formalMap f g h a = Formal
  (f $ l2 a)
  (g $ l1 a)
  (h $ l0 a)



formalZipWith :: (L a2 -> L b2 -> L c2) 
              -> (L a1 -> L b1 -> L c1) 
              -> (L a0 -> L b0 -> L c0)
              -> Formal a2 a1 a0
              -> Formal b2 b1 b0
              -> Formal c2 c1 c0
formalZipWith f g h !a !b = Formal
  (f (l2 a) (l2 b))
  (g (l1 a) (l1 b))
  (h (l0 a) (l0 b))

formalZipWith3 :: (L a2 -> L b2 -> L c2 -> L d2) 
               -> (L a1 -> L b1 -> L c1 -> L d1) 
               -> (L a0 -> L b0 -> L c0 -> L d0)
               -> Formal a2 a1 a0
               -> Formal b2 b1 b0
               -> Formal c2 c1 c0
               -> Formal d2 d1 d0
formalZipWith3 f g h a b c = Formal
  (f (l2 a) (l2 b) (l2 c))
  (g (l1 a) (l1 b) (l1 c))
  (h (l0 a) (l0 b) (l0 c))

formalIdentify :: Formal a b c -> Formal (Idd a) (Idd b) (Idd c)
formalIdentify (Formal a b c) = 
  Formal (identify a)
         (identify b)
         (identify c)