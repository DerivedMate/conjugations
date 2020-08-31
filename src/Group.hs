{-# LANGUAGE OverloadedStrings #-}


module Group where

import Control.Applicative (empty)
import Data.Aeson
import Data.List

data Group a g = Group [a] g
  deriving (Show)

instance Semigroup (Group a g) where
  (<>) (Group as g) (Group bs _) = Group (as <> bs) g

instance (Eq a, Eq g) => Eq (Group a g) where
  (==) (Group as ga) (Group bs gb) = as == bs && ga == gb

instance (ToJSON a, ToJSON g) => ToJSON (Group a g) where
  toJSON (Group as g) = object 
    [ "members" .= as
    , "group"   .= g
    ]

  toEncoding (Group as g) = pairs 
    $  "members" .= as
    <> "group"   .= g
 
instance (FromJSON a, FromJSON g) => FromJSON (Group a g) where
  parseJSON (Object v) = Group <$>
                        v .: "members" <*>
                        v .: "group"

  parseJSON _          = empty

groupCategory :: Group a g -> g
groupCategory (Group _ g)  = g

groupMembers :: Group a g -> [a]
groupMembers  (Group as _) = as

groupElem :: Eq a => a -> Group a g -> Bool
groupElem a g = a `elem` groupMembers g

joinGroup :: Eq a => (g -> Bool) -> a -> Group a g -> Group a g
joinGroup test a (Group as g)
  | a `notElem` as && test g = Group (a:as) g
  | otherwise                = Group as g

mapGroup :: (g -> h) -> [Group a g] -> [Group a h]
mapGroup f as = map aux as
  where aux (Group as g) = Group as (f g)

cmpGroups :: (Eq a, Eq g) => Group a g -> Group a g -> Bool
cmpGroups (Group _ a) (Group _ b) = a == b

