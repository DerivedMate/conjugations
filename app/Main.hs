module Main where

import Scraper
import Helpers
import Ling
import Group
import Control.Monad 
import Data.List
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson.Encode.Pretty
import System.IO

tierOfGroup :: Group a (L3 g) -> Int
tierOfGroup g = sum $ map length $ groupCategory g

lengthOfOutput :: FilePath -> IO ()
lengthOfOutput path = do
  out <- openFile path ReadMode
         >>= BL.hGetContents
  writeFile "data.json" 
    $ show
    $ reverse 
    $ sortOn length 
    $ map (map (length . groupMembers)) 
    $ groupBy g 
    $ sortOn tierOfGroup
    $ unpackMaybeList (decode out :: Maybe [Group String (L3 Category)])
  where g a b = tierOfGroup a == tierOfGroup b

main :: IO ()
main = readLines "verbsList.txt" 
  >>= pure . zip [0..]
  >>= foldM processVerb []
  >>= pure . reverse . sortOn (length . groupMembers)
  >>= BL.putStr . encodePretty
  -- >>= removeNonVerbs
  -- >>= pure . drop 500
  -- >>= mapM_ testVerb
  -- >>= pure . zip [0..]
  -- >>= foldM processVerbs []
  -- >>= pure . sortOn (\(Indexed c _) -> c)
  -- >>= print
  -- >> putStrLn "_\tfinish"
  {-(
    \f -> extractConjugations f 
          >>= print . map (map stemWIrreg) 
  )-}

       -- extractConjugationLinks "spanish"
       -- >>= downloadVerbs "verbs/spanish2" 
       -- >> print "done"