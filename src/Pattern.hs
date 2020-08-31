{-# LANGUAGE OverloadedStrings #-}


module Pattern where

import Control.Monad
import Control.Applicative
import Data.Aeson
  
type Conjugation = [String]
type Suffixes    = [String]
type Irreg       = (Int, Char)
type Infinitive  = String
-- baseStem, [irregularity], suffix
data Pattern     = Pattern String [Irreg] String
  deriving (Show, Eq)
-- A non-specific version of `Pattern`
type Verb        = (Infinitive, [[[Pattern]]]) -- ¿?
{- v1: 
  ex: [poder, puedo] -> [ Pattern (pd, [(0, o)], er) 
                        , Pattern (pd, [(0, u), (1, e)], o) 
                        ]
                     -> Transform (0, o, ue)

   v2: Discarding the idea of irregularities, there is no base form; and therefore, no transformed ones, thus making the previous idea unfeasible.
      (note: the example takes into account the whole present indicative conjugation (hence, 'e' is not a part of the common part), but only goes over two words: puede and podemos)
   ex: [puede, podemos] -> [ Pattern "pd" [(0, 'u'), (1, 'e')] "e"
                           , Pattern "pd" [(0, 'o')] "emos"
                           ]
                         -> [ Category ["ue"] "e"
                            , Category ["o"] "emos"
                            ]     
-}
type Transform   = String
data Category    = Category [Transform] String
  deriving (Eq, Show)

instance ToJSON Category where
  toJSON (Category ts suffix) = object 
    [ "transformations" .= ts
    , "suffix"          .= suffix
    ]
  
  toEncoding (Category ts suffix) = pairs  
    $  "transformations" .= ts
    <> "suffix"          .= suffix
    
instance FromJSON Category where
  parseJSON (Object v) = Category 
    <$> v .: "transformations" 
    <*> v .: "suffix"

  parseJSON _          = empty