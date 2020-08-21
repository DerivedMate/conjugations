{-# LANGUAGE OverloadedStrings #-}

module Scraper where

import Network.HTTP.Conduit
import Control.Monad
import Data.List
import Helpers
import Ling
import System.IO
import System.Directory
import Text.HTML.Scalpel
import Data.Char
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as B8
import Group

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
                      

downloadVerbs :: String -> [String] -> IO ()
downloadVerbs dist_ urls = 
  mapM_ aux urls
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

extractConjugations :: FilePath -> IO [[[String]]]
extractConjugations file = 
  openFile file ReadMode
  >>= hGetContents
  >>= pure 
      . unpackMaybeList 
      . split
  >>= pure 
      . groupByLengths
      . partitionToLengths 
        [ 3           -- 1x3
        , 5,5,5,5,5,5 -- 6x5
        , 4,4,4,4,4,4 -- 6x4
        , 2,2,2,2,2 ] -- 5x2
  >>= pure . map transpose
  where 
    isStringLower str = all (isSpace <||> isLower) str
    split html        = do
      v  <- inf html
      vs <- aux html
      pure (v:vs) 
    aux html          = scrapeStringLike html scraper
    inf html          = scrapeStringLike html infinitiveScraper
    condition w       = (isStringLower w) && (not $ elem w toExclude)
    toExclude         = ["yo", "tú", "nosotros", "vosotros", ""]
    predicates        = map hasClass ["_1btShz4h", "_3HlR1KX5", "P-yt87tk","_1b8PdQhU"]
    infinitiveScraper :: Monad m0 => ScraperT String m0 String
    infinitiveScraper = chroot ("h1" @: [hasClass ("_1xnuU6l-")]) $ text anySelector 
    scraper           = chroots ("a" @: predicates) $ do
                          word  <- text anySelector
                          word' <- (pure . splitAtEl ',') word
                          guard (condition word')
                          pure word'


testVerb :: String -> IO ()
testVerb file = 
  extractConjugations file
  >>= f -- print . aux
  where 
    f a          = catch (pure $ printer $ g a) errHandler 
                   >>= dp
    errHandler :: ConjugationError -> IO String          
    errHandler e = pure $ show e
    dp a = do
      putStrLn a
      hPutStrLn stderr a
    printer r 
      | r         = verb <> "\t" <> "Success"
      | otherwise = verb <> "\t" <> "!!FAIL!!"
    
    verb    = takeWhile isLetter 
              $ dropWhileWhole (elem '/') file
    g ws_ = ws' == ws
      where 
        ws' = map 
              (map ( (map assemblePattern) . stemWIrreg ))
              ws
        ws  = normalizeConjugations ws_
        
    aux ws_ = map 
              (map ( (map assemblePattern) . stemWIrreg ))
              (normalizeConjugations ws_)
    

processVerbs :: [Indexed (Embedded Category)] -> (Int, FilePath) -> IO [Indexed (Embedded Category)]
processVerbs cs (n, path) = do
  cons       <- extractConjugations path
  categories <- pure 
                $ categoryOfVerb
                $ map 
                  (map stemWIrreg) 
                  (normalizeConjugations cons)

  hPutStrLn stderr ("[" <> show n <> "] " <> path)

  let cs' = map (aux categories) cs
    in pure $ if cs' == cs then (Indexed 1 categories) : cs
              else cs'
  where 
    verb = takeWhile isLetter 
           $ dropWhileWhole (elem '/') path
    aux :: Embedded Category -> Indexed (Embedded Category) -> Indexed (Embedded Category)
    aux a b@(Indexed count a')
      | a == a'   = Indexed (count + 1) a'
      | otherwise = b

type PVT = [Group String (L3 Category)]

processVerb :: PVT -> (Int, FilePath) -> IO PVT
processVerb gs f@(i, path) = do
  cons       <- extractConjugations path
  categories <- pure 
                $ categoryOfVerb
                $ map 
                  (map stemWIrreg) 
                  (normalizeConjugations cons)
  pure $ foldl aux gs (formalize categories)
  <* hPutStrLn stderr ("[" <> show i <> "] " <> path) 

  where
    verb  = takeWhile isLetter 
            $ dropWhileWhole (elem '/') path

    aux :: [Group String (L3 Category)] -> L3 Category -> [Group String (L3 Category)]
    aux gs c 
      | c `elem` (map groupCategory gs) = 
        map (joinGroup (c ==) verb) gs
      | otherwise =
        (Group [verb] c) : gs
  
removeNonVerbs :: [FilePath] -> IO ()
removeNonVerbs fs = mapM_ aux fs
  where
    determinant = "subjunctive"
    aux f = do
      exists <- doesFileExist f
      txt <- openFile f ReadMode >>= hGetContents

      when (exists && (not $ isInfixOf determinant txt)) $ do 
        putStrLn ("removing: " <> f)
        removeFile f