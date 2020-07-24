{-# LANGUAGE OverloadedStrings #-}

module Scraper where

import Network.HTTP.Conduit
import Control.Monad
import Data.List
import Helpers
import Ling
import System.IO
import Text.HTML.Scalpel
import Data.Char
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B8

downloadPage :: String -> String -> IO ()
downloadPage url dist =
  simpleHttp url
  >>= (B8.writeFile dist)

downloadPageConditional :: String -> String -> (String -> Bool) -> IO ()
downloadPageConditional url dist f =
  simpleHttp url >>= aux
  where
    aux s
      | f s'      = B8.writeFile dist s
      | otherwise = mempty
      where s'    = B8.unpack s


downloadSpanishDict :: String -> IO [()]
downloadSpanishDict dist = 
  sequence $ map (aux . stringOfChar) letters 
  where 
    distN   = normalizePath dist
    baseUrl = "http://www.spanishdict.com/sitemap/dictionary/spanish/"
    letters = ['a'..'z']
    aux l   = downloadPage 
              (baseUrl <> l)
              (dist <> "/" <> l <> ".html") 

extractConjugationLinks :: String -> IO [String]
extractConjugationLinks dist_ = 
  sequence (map aux letters)
  >>= pure . concat . map unpackMaybeList
  where
    linkPrefix    = "https://www.spanishdict.com/conjugate"
    dist          = normalizePath dist_
    letters       = map stringOfChar ['a'..'z']
    aux l         = openFile 
                      (dist <> "/" <> l <> ".html") 
                      ReadMode
                    >>= hGetContents 
                    >>= getLinks
    getLinks html = pure $ scrapeStringLike html scraper
    condition     = isInfixOf "/translate" <&> isVerbSpanish
    scraper       = chroots "a" $ do
                      href <- attr "href" anySelector
                      guard (condition href)
  
                      pure $ linkPrefix <> (drop 10 href)
                      

downloadVerbs :: String -> [String] -> IO [()]
downloadVerbs dist_ urls = 
  sequence 
  $ map aux 
  {-$ drop 1 
  $ dropWhile (/= (prefix <> "vaticinar"))-} urls
  where 
    prefix  = "https://www.spanishdict.com/conjugate/" :: String
    dist    = normalizePath dist_
    aux url = downloadPageConditional 
                url 
                (dist <> "/" <> file <> ".html") 
                (isInfixOf "Participle")
              >> putStrLn file
              where 
                file = drop (length prefix) url

extractConjugations :: String -> FilePath -> IO [[[String]]]
extractConjugations dist_ file = 
  openFile file ReadMode
  >>= hGetContents
  >>= pure 
      . unpackMaybeList 
      . aux
  >>= pure 
      . groupByLengths
      . partitionToLengths 
        [ 2           -- 1x2
        , 5,5,5,5,5,5 -- 6x5
        , 4,4,4,4,4,4 -- 6x4
        , 2,2,2,2,2 ] -- 5x2
  >>= pure . map transpose
  where 
    dist              = normalizePath dist_
    isStringLower str = all (isSpace <||> isLower) str
    aux html          = scrapeStringLike html scraper
    condition w       = (isStringLower w) && not (w `elem` toExclude)
    toExclude         = ["yo", "tú", "nosotros", "vosotros", ""]
    predicates        = map hasClass ["_1btShz4h", "_3HlR1KX5", "P-yt87tk","_1b8PdQhU"]
    scraper           = chroots ("a" @: predicates) $ do
                          word <- text anySelector
                          guard (condition word)
                          pure word


testVerb :: String -> IO ()
testVerb v = 
  extractConjugations "" ("verbs/spanish2/" <> v <> ".html") 
  >>= print . aux
  where 

    aux ws = ws' == ws
      where ws' = map 
                (map ((map assemblePattern) . stemWIrreg))
                ws
    
   