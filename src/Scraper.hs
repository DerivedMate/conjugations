{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Scraper where

import Network.HTTP.Conduit
import Control.Monad
import Data.List
import Helpers
import Ling
import Group
import Pattern
import Formal
import System.IO
import System.Directory
import Text.HTML.Scalpel
import Data.Char
import Control.Exception
import Debug.Trace
import System.Mem
import qualified Data.ByteString.Lazy.Char8 as B8

type PVT = Formal
            (Group String (L3 Category))
            (Group String (L2 Category))
            (Group String (L  Category))

downloadPage :: String -> String -> IO ()
downloadPage url dist =
  simpleHttp url
  >>= B8.writeFile dist

downloadPageConditional :: String -> String -> (String -> Bool) -> IO ()
downloadPageConditional url dist f =
  simpleHttp url >>= aux
  where
    aux s
      | f s'      = B8.writeFile dist s
      | otherwise = mempty
      where s'    = B8.unpack s


downloadSpanishDict :: String -> IO ()
downloadSpanishDict dist = 
  mapM_ (aux . stringOfChar) letters 
  where 
    distN   = normalizePath dist
    baseUrl = "http://www.spanishdict.com/sitemap/dictionary/spanish/"
    letters = ['a'..'z']
    aux l   = downloadPage 
              (baseUrl <> l)
              (dist <> "/" <> l <> ".html") 

extractConjugationLinks :: String -> IO [String]
extractConjugationLinks dist_ = 
  mapM aux letters
  >>= pure . concatMap unpackMaybeList
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

                      -- Drops "/translate" and attaches the rest to the `linkPrefix`
                      pure $ linkPrefix <> drop 10 href
                      

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
        , 2,2,2,2,2   -- 5x2
        ] 
  >>= pure . map transpose
  where 
    isStringLower str = all (isSpace <||> isLower) str
    split html        = do
      v  <- inf html
      vs <- aux html
      pure (v:vs) 
    aux html          = scrapeStringLike html scraper
    inf html          = scrapeStringLike html infinitiveScraper
    condition w       = (isStringLower w) && (notElem w toExclude)
    toExclude         = ["yo", "tú", "nosotros", "vosotros", ""]
    predicates        = map hasClass ["_1btShz4h", "_3HlR1KX5", "P-yt87tk","_1b8PdQhU"]
    infinitiveScraper :: Monad m0 => ScraperT String m0 String
    infinitiveScraper = chroot ("h1" @: [hasClass "_1xnuU6l-"]) $ text anySelector 
    scraper           = chroots ("a" @: predicates) $ do
                          word  <- text anySelector
                          word' <- (pure . splitAtEl ',') word
                          guard (condition word')
                          pure word'

inspectVerb :: String -> IO ()
inspectVerb file =
  extractConjugations file
  >>= print . aux
  where
    aux ws_ = map 
              (map ( map assemblePattern . stemWIrreg ))
              (normalizeConjugations ws_)

testVerb :: String -> IO ()
testVerb file = 
  extractConjugations file
  >>= f
  where 
    f a           = pure (printer $ g a)
                    >>= doublePrint
    doublePrint a = do
                    putStrLn a
                    hPutStrLn stderr a
    printer r 
      | r         = verb <> "\t" <> "Success"
      | otherwise = verb <> "\t" <> "!!FAIL!!"
    
    verb          = takeWhile isLetter 
                    $ dropWhileWhole (elem '/') file
    g ws_         = ws' == ws
      where 
        ws' = map 
              (map ( map assemblePattern . stemWIrreg ))
              ws
        ws  = normalizeConjugations ws_
           
processVerb :: Maybe PVT -> (Int, FilePath) -> IO (Maybe PVT)
processVerb f0 (i, path) = do
  cons       <- extractConjugations path
 
  let 
    categories = makeFormal
                 $ categoryOfVerb
                 $ map 
                   (map stemWIrreg) 
                   (normalizeConjugations cons)
    verb       = verbalize path
    mapF       = map (Group [verb])
    f1         = formalMap mapF mapF mapF categories
    in pure $ Just $ 
      case f0 of
        Just f0 -> mergeFormals f0 f1
        Nothing -> f1
    
mergeFormals :: PVT -> PVT -> PVT
mergeFormals (Formal !a2 !a1 !a0) (Formal !b2 !b1 !b0) = 
  Formal (a2 `aux` b2) (a1 `aux` b1) (a0 `aux` b0)
  where 
    aux :: (Eq a, Eq g) => [Group a g] -> [Group a g] -> [Group a g]
    aux gsa gsb = foldr merge gsa gsb

    merge :: (Eq a, Eq g) => Group a g -> [Group a g] -> [Group a g]
    merge ga gsb
      | gsb' == gsb = ga : gsb'
      | otherwise   = gsb'
      where gsb'    = map (mergeGroups ga) gsb
    
    mergeGroups :: Eq g => Group a g -> Group a g -> Group a g
    mergeGroups (Group msa ga) (Group msb gb)
      | groupsMatch     = Group (msa ++ msb) ga
      | otherwise       = Group msb gb
      where groupsMatch = ga == gb

removeNonVerbs :: [FilePath] -> IO ()
removeNonVerbs fs = mapM_ aux fs
  where
    determinant = "subjunctive"
    aux f       = do
      exists <- doesFileExist f
      txt    <- openFile f ReadMode 
                >>= hGetContents

      when (exists && not (determinant `isInfixOf` txt)) $ do 
        putStrLn ("removing: " <> f)
        removeFile f