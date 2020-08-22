module Main where

import Scraper
import Helpers
import Ling
import Group
import Pattern
import Control.Monad 
import Data.List
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson.Encode.Pretty
import System.IO
import Data.Ord

tierOfGroup :: Group a (L3 g) -> Int
tierOfGroup g = sum $ map length $ groupCategory g

lengthOfOutput :: FilePath -> FilePath -> IO ()
lengthOfOutput path outPath = do
  out <- openFile path ReadMode
         >>= BL.hGetContents
  writeFile outPath
    $ show
    $ sortOn (Down . length)
    $ map (map (length . groupMembers)) 
    $ groupBy g 
    $ sortOn tierOfGroup
    $ unpackMaybeList (decode out :: Maybe [Group String (L3 Category)])
  where g a b = tierOfGroup a == tierOfGroup b

cmpFiles a b = do
  fa <- openFile a ReadMode >>= BL.hGetContents
  fb <- openFile b ReadMode >>= BL.hGetContents

  let 
    va = decode fa :: Maybe [[Int]]
    vb = decode fb :: Maybe [[Int]]
    in pure $ va == vb

main :: IO ()
main = readLines "verbsList.txt" 
  >>= pure . zip [0..]
  >>= foldM processVerb []
  >>= pure . sortOn (Down . length . groupMembers)
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