module Main where

import Scraper
import Control.Monad 

main :: IO ()
main = extractConjugationLinks "spanish"
       >>= downloadVerbs "verbs/spanish2" 
       >> print "done"