{-# LANGUAGE OverloadedStrings #-}


module Group where

import Control.Applicative (empty)
import Data.Aeson
import Data.List

data Group a g = Group [a] g
  deriving (Eq, Show)
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
groupElem a g = elem a (groupMembers g)

joinGroup :: Eq a => (g -> Bool) -> a -> Group a g -> Group a g
joinGroup test a (Group as g)
  | (not . elem a) as && test g = Group (a:as) g
  | otherwise                   = Group as g

mapGroup :: (g -> h) -> [Group a g] -> [Group a h]
mapGroup f as = map aux as
  where aux (Group as g) = Group as (f g)

concatGroups :: Group a g -> Group a g -> Group a g
concatGroups (Group a0 g0) (Group a1 _) = Group (concat [a0, a1]) g0