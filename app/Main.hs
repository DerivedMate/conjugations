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
  >>= pure . zip [0..]
  >>= foldM processVerb Nothing
  >>= BL.putStr . encodePretty 