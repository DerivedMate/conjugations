module Main where

import Scraper
import Helpers
import Ling
import Group
import Pattern
import Formal
import Control.Monad 
import Data.List
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson.Encode.Pretty
import System.IO
import Data.Ord

doSorting :: PVT -> PVT
doSorting (Formal l2 l1 l0) = Formal (sortFormal l2) (sortFormal l1) (sortFormal l0)
  where 
    nMembers :: Idd (Group String a) -> Int
    nMembers (Idd _ (Group ms _)) = length ms

    sortFormal :: [Idd (Group String a)] -> [Idd (Group String a)]
    sortFormal as = sortOn (Down . nMembers) as

sortData :: FilePath -> IO ()
sortData path = do
  fh      <- openFile path ReadMode
  content <- BL.hGetContents fh

  BL.putStr $ encodePretty $ doSorting <$> decode content

    

cmpFiles :: FilePath -> FilePath -> IO Bool
cmpFiles a b = do
  fa <- openFile a ReadMode >>= BL.hGetContents
  fb <- openFile b ReadMode >>= BL.hGetContents

  let 
    va = decode fa :: Maybe [[Int]]
    vb = decode fb :: Maybe [[Int]]
    in pure $ va == vb

main :: IO ()
main = 
  readLines "verbsList.txt" 
  >>= mapM (processVerb extractConjugations)
  >>= pure . foldl1 mergeFormals
  >>= pure . doSorting
  >>= BL.putStr . encodePretty 
