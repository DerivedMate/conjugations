module Ling where

import Data.List
import qualified Codec.Binary.UTF8.String as UT
import Helpers

type Conjugation = [String]
type Suffixes    = [String]
type Irreg       = (Int, Char)
-- baseStem, [irregularity], suffix
type Pattern     = (String, [Irreg], String) 

stem :: Conjugation -> String
stem cs = aux (cs !! 0) cs
  where
    n0 = length cs
    aux pf wds
      | matches == n0 = pf 
      | otherwise     = aux (init pf) wds
      where 
          matches = length 
                    $ filter (/= Nothing)
                    $ map (stripPrefix pf) wds

assemblePattern :: Pattern -> String
assemblePattern (base, irs, suffix) =
  foldl aux (base <> suffix) irs
  where 
    aux w (i, l) = insertAt l i w

-- Merge adjacent irregularities (?)
stemWIrreg :: Conjugation -> [Pattern]
stemWIrreg ws = map (aux [] 0 baseStem) ws
  where
    baseStem = foldl1 intersect' ws
    aux rest i base w
      | isExhausted         = (baseStem, reverse rest, w)
      | head base == head w = aux rest (i+1) (tail base) (tail w)
      | otherwise           = aux ((i-1, head w):rest) (i+1) base (tail w)
      where 
        isExhausted = length base == 0 
                      || length w == 0



conn :: IO ()
conn = print $ map (\\ (stem words)) words
  where 
    words = [ "robię"  , "robisz"
            , "robi"   , "robimy"
            , "robicie", "robią"
            ] 

-- checks whether a given word is a Spanish verb (excluding reflexive ones)
isVerbSpanish :: String -> Bool
isVerbSpanish str = 
  let str' = UT.decodeString str
  in or [ isSuffixOf ending str' 
     && not (isInfixOf "%20" str')
     | ending <- ["ar", "er", "ér", "ir", "ír"]
     ]

