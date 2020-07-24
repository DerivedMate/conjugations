module Main where

import Scraper
import Control.Monad 

main :: IO ()
main = readLines "verbsList.txt" 
  >>= mapM_ (
    \f -> extractConjugations "" f 
          >>= print . map (map stemWIrreg) 
  )

       -- extractConjugationLinks "spanish"
       -- >>= downloadVerbs "verbs/spanish2" 
       -- >> print "done"