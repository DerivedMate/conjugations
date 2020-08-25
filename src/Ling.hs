{-# LANGUAGE OverloadedStrings #-}

module Ling where

import Data.List
import Data.Typeable
import Data.Aeson
import Control.Exception
import Control.Applicative (empty)
import Debug.Trace
import Helpers
import qualified Arrow as A
import Pattern

data ConjugationError = ConjugationError [[Conjugation]]
  deriving (Show, Typeable)

instance Exception ConjugationError

patternCommon :: Pattern -> String
patternCommon (Pattern c _ _) = c

assemblePattern :: Pattern -> String
assemblePattern (Pattern base irs suffix) =
  foldl aux (base <> suffix) irs
  where 
    aux w (i, l) = insertAt l i w

stemWIrreg :: Conjugation -> [Pattern]
stemWIrreg ws = map (`A.common` baseStem) ws
  where
    baseStem                = foldl1 f ws
    f a b                   = patternCommon $ A.common a b

-- checks whether a given word is a Spanish verb (excluding reflexive ones)
isVerbSpanish :: String -> Bool
isVerbSpanish str = 
  or [ ending `isSuffixOf` str
     | ending <- ["ar", "er", "ér", "ir", "ír"]
     ] && not ("%20" `isInfixOf` str)

normalizeConjugations :: [[Conjugation]] -> [[Conjugation]]
normalizeConjugations (ps:ws) = [concat ps] : ws
normalizeConjugations ws      = throw $ ConjugationError ws

categoryOfPattern :: Pattern -> Category
categoryOfPattern (Pattern _ ir s) = Category (aux [] ir) s
  where
    aux ts []              = reverse (map snd ts)
    aux [] ((i, r):irs)    = aux [(i, [r])] irs
    aux (t@(i, t'):ts) ((j, ir):irs)
      | length t' + i == j = aux ((i, t' <> [ir]):ts) irs
      | otherwise          = aux ((j, [ir]) : t:ts) irs

categoryOfVerb :: [[[Pattern]]] -> [[[Category]]]
categoryOfVerb cs = map (map (map categoryOfPattern)) cs