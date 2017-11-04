{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( runSample,
    runCrawl,
    Configuration(..)
  ) where

import           Network.HTTP.Client        (HttpException)
import           Network.URI
import           Network.Wreq               (get, responseBody)

import           Data.List                  (intercalate, isPrefixOf)
import           Data.List.Split            (splitOn)
import           Data.Maybe                 (fromMaybe, mapMaybe, isJust, fromJust)
import           Data.Set                   (Set, deleteAt, difference, elemAt,
                                             empty, fromList, insert, null,
                                             singleton, union)

import           Data.Text.Format           (Only (..), print)

import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath.Posix      (dropDrive, (</>))

import qualified Control.Exception          as E
import           Control.Lens
import           Control.Monad              (when)

import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Text.HTML.TagSoup as TS

import           Prelude                    hiding (null, print)

-- | current state of crawling
data CrawlerState = CrawlerState
  { linksPending :: Set URI
  , linksCrawlin :: Set URI
  , linksVisited :: Set URI
  } deriving (Show)

data Configuration = Configuration
  { startUri :: URI
  , concurrencyLevel :: Int
  , visitedLinkLimit :: Int
  }

runSample :: IO ()
runSample = do
  let sampleUriMaybe = parseURIReference "https://en.wikipedia.org/wiki/Alexander_Pushkin"
  if isJust sampleUriMaybe
    then runCrawl Configuration {
      startUri = fromJust sampleUriMaybe,
      concurrencyLevel = 1,
      visitedLinkLimit = 100
    }
    else putStrLn "Invalid start URI"

-- | perform crawling, starting with rootUri
runCrawl :: Configuration -> IO ()
runCrawl cfg = do
  print "start crawling from {}\n" (Only $ show $ startUri cfg)
  print "concurrency level: {}, visited link limit: {}\n" (concurrencyLevel cfg, visitedLinkLimit cfg)

  let state = CrawlerState {
    linksPending = singleton $ startUri cfg,
    linksCrawlin = empty,
    linksVisited = empty
  }

  newState <- crawlLoop cfg state

  print "Done. {} links visited.\n" (Only $ length $ linksVisited newState)

crawlLoop :: Configuration -> CrawlerState -> IO CrawlerState
crawlLoop cfg state = do
  newState <- crawlOne state
  if null (linksPending newState) || length (linksVisited newState) > (visitedLinkLimit cfg)
    then return newState
    else crawlLoop cfg newState

crawlOne :: CrawlerState -> IO CrawlerState
crawlOne state =
  if null pending
    then return state
    else do
      let lnk = elemAt 0 pending
          visited = linksVisited state

      page <- getPage lnk `E.catch` handler
      dumpPageToFile lnk page
      links <- getLinksFromPage lnk page

      let newVisited = insert lnk visited
          newPending = deleteAt 0 pending
          linksWithoutVisited = difference links newVisited
          linksWithoutPending = difference linksWithoutVisited newPending

      return state {
        linksVisited = newVisited,
        linksPending = newPending `union` linksWithoutPending
      }
  where
    pending = linksPending state
    handler :: HttpException -> IO BS.ByteString
    handler e = do
      print "{}" (Only $ show e)
      return $ BS.pack ""

-- | Retrieves a page from the Web
getPage :: URI -> IO BS.ByteString
getPage uri = do
  r <- get $ show uri
  return $ r ^. responseBody

-- | Retrieves all the outgoing links from a web page
getLinksFromPage :: URI -> BS.ByteString -> IO (Set URI)
getLinksFromPage uri body = do
  let tags = TS.parseTags body
      as = filter (TS.isTagOpenName "a") tags
      hrefs = map (TS.fromAttrib "href") as
      links = mapMaybe (transformToAbsolute uri) hrefs
      isOutgoing link = uriPath uri /= uriPath link
      sameDomain link = fromMaybe False (fromSameDomain uri link)
      filteredLinks = filter (\link -> isOutgoing link && sameDomain link) links

  -- leave out only outgoing links
  return $ fromList filteredLinks

--  mapM_ print filteredLinks
fromSameDomain :: URI -> URI -> Maybe Bool
fromSameDomain one two = do
  authOne <- uriAuthority one
  authTwo <- uriAuthority two
  return (uriRegName authOne == uriRegName authTwo)

-- | transforms relative links to absolute ones
transformToAbsolute :: URI -> BS.ByteString -> Maybe URI
transformToAbsolute baseUri linkStr = do
  -- TODO use uri-bytestring here
  link <- parseURIReference (BS.unpack linkStr)
  -- intentionally truncate query params
  return (link {uriQuery = ""} `relativeTo` baseUri)

baseOutputDir = "output"

dumpPageToFile :: URI -> BS.ByteString -> IO ()
dumpPageToFile pageUri body = do
  let (dirName, fileName) = uriToDirAndFileName pageUri
      outputDir = baseOutputDir </> dropDrive dirName
      outputFile = outputDir </> fileName
  createDirectoryIfMissing True outputDir
  BS.writeFile outputFile body

  putStrLn $ "writted to disk: " ++ (show pageUri)

uriToDirAndFileName :: URI -> (FilePath, FilePath)
uriToDirAndFileName uri =
  let path = uriPath uri
      parts = splitOn "/" path
  in (intercalate "/" (init parts), last parts)
