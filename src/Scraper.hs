-- {-# LANGUAGE OverloadedStrings #-}

module Scraper where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as B8
import Control.Monad
import Data.List
import qualified Data.Text as T
import Helpers
import Ling
import System.IO
import Text.HTML.TagSoup

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
  >>= pure . concat
  where
    letters = [stringOfChar x | x <- ['a'..'z']]
    dist    = normalizePath dist_
    aux l   = 
      openFile 
        (dist <> "/" <> l <> ".html") 
        ReadMode
      >>= hGetContents
      >>= pure . parseTags
      >>= pure . sections (~== "<a>")
      >>= pure . map (fromAttrib "href" . head)
      >>= pure . filter (isPrefixOf "/translate" <&> isVerbSpanish)
      >>= pure . map (drop 10) -- drops "/translate"
      >>= pure . map ("https://www.spanishdict.com/conjugate" <>)

downloadVerbs :: String -> [String] -> IO [()]
downloadVerbs dist_ urls = 
  sequence 
  $ map aux 
  $ drop 1 
  $ dropWhile (/= (prefix <> "desguarnecer")) urls
  where 
    prefix  = "https://www.spanishdict.com/conjugate/"
    dist    = normalizePath dist_
    aux url = downloadPageConditional 
                url 
                (dist <> "/" <> file <> ".html") 
                (isInfixOf "Participle")
              >> putStrLn file
              where 
                file = drop (length prefix) url