module Main where

import Scraper
import Helpers
import Ling
import Control.Monad 

main :: IO ()
main = readLines "verbsList.txt" 
  -- >>= removeNonVerbs
  -- >>= pure . drop 500
  >>= mapM_ testVerb
  >> putStrLn "_\tfinish"
  {-(
    \f -> extractConjugations f 
          >>= print . map (map stemWIrreg) 
  )-}

       -- extractConjugationLinks "spanish"
       -- >>= downloadVerbs "verbs/spanish2" 
       -- >> print "done"