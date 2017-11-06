{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
  ( runSample,
    runCrawl,
    Configuration(..)
  ) where

import           Network.HTTP.Client        (HttpException)
import           Network.URI
import           Network.Wreq               (get, responseBody)

import           Data.List                  (intercalate, isPrefixOf)
import qualified Data.List                  as L
import           Data.List.Split            (splitOn)
import           Data.Maybe                 (fromJust, fromMaybe, isJust,
                                             mapMaybe)
import qualified Data.Set                   as S (Set, delete, deleteAt,
                                                  difference, elemAt, empty,
                                                  fromList, insert, null,
                                                  singleton, toList, union)

import           Data.Text.Format           (Only (..), print)

import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath.Posix      (dropDrive, (</>))

import qualified Control.Exception          as E
import           Control.Lens               ((^.))
import           Control.Monad              (when)

import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Text.HTML.TagSoup          as TS

import           Control.Concurrent
import           Control.Concurrent.Async
import           Prelude                    hiding (null, print)

-- | current state of crawling
data CrawlerState = CrawlerState
  { linksPending :: [URI] -- TODO use Seq here?
  , linksCrawlin :: S.Set URI
  , linksVisited :: S.Set URI
  } deriving (Show)

-- | crawler configuration
data Configuration = Configuration
  { startUri         :: URI
  , concurrencyLevel :: Int
  , visitedLinkLimit :: Int
  }

type AsyncJob = Async (URI, S.Set URI)

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
runCrawl cfg@Configuration{..} = do
  print "start crawling from {}\n" (Only $ show startUri)
  print "concurrency level: {}, visited link limit: {}\n" (concurrencyLevel, visitedLinkLimit)

  let state = CrawlerState {
    linksPending = [startUri],
    linksCrawlin = S.empty,
    linksVisited = S.empty
  }

  CrawlerState{linksVisited} <- crawlThreaded cfg state

  print "Done. {} links visited.\n" (Only $ length linksVisited)

-- | crawl using threaded implementation
crawlThreaded :: Configuration -> CrawlerState -> IO CrawlerState
crawlThreaded = crawlLoopThreaded []

crawlLoopThreaded :: [AsyncJob] -> Configuration -> CrawlerState -> IO CrawlerState
crawlLoopThreaded jobs cfg@Configuration{..} state@CrawlerState{..}
  -- nothing more to do
  | L.null linksPending && S.null linksCrawlin = return state
  -- we've hit visited link limit
  | length linksVisited > visitedLinkLimit   = return state
  -- limit concurrency or wait for more work to do
  | L.null linksPending || L.length jobs > concurrencyLevel = do
          (job, (lnk, crawled)) <- waitAny jobs

          let newState = visitedOne lnk crawled state

          crawlLoopThreaded (L.delete job jobs) cfg newState
  -- just schedule another one
  | otherwise = do
          let (lnk, newState) = popOneFromPending state

          job <- async $ crawlLink lnk

          crawlLoopThreaded (job : jobs) cfg newState


-- | single threaded implementation
crawlLoop :: Configuration -> CrawlerState -> IO CrawlerState
crawlLoop cfg@Configuration{..} state = do
  newState@CrawlerState{..} <- crawlOne state
  if L.null linksPending || length linksVisited  > visitedLinkLimit
    then return newState
    else crawlLoop cfg newState

crawlOne :: CrawlerState -> IO CrawlerState
crawlOne state@CrawlerState{linksPending} =
  if L.null linksPending
    then return state
    else do
      let (lnk, newState) = popOneFromPending state

      (_, links) <- crawlLink lnk

      return $ visitedOne lnk links state

-- | pop one link from pending into crawling
popOneFromPending :: CrawlerState -> (URI, CrawlerState)
popOneFromPending state@CrawlerState{linksPending, linksCrawlin} =
    (lnk, newState)
  where
    lnk : otherPending = linksPending
    newState = state {
          linksPending = otherPending,
          linksCrawlin = S.insert lnk linksCrawlin
    }

-- | mark one link as completed and process crawling results
visitedOne :: URI -> S.Set URI -> CrawlerState -> CrawlerState
visitedOne lnk crawledLinks state@CrawlerState{..} =
      state {
        linksVisited = S.insert lnk linksVisited,
        linksPending = linksPending ++ filter (isNewLink state) (S.toList crawledLinks),
        linksCrawlin = S.delete lnk linksCrawlin
      }

isNewLink :: CrawlerState -> URI -> Bool
isNewLink CrawlerState{..} lnk =
  lnk `notElem` linksVisited &&
  lnk `notElem` linksPending &&
  lnk `notElem` linksCrawlin

-- | crawl the link
crawlLink :: URI -> IO (URI, S.Set URI)
crawlLink lnk = do
      page <- getPage lnk `E.catch` handler
      dumpPageToFile lnk page
      crawled <- getLinksFromPage lnk page
      return (lnk, crawled)
  where
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
getLinksFromPage :: URI -> BS.ByteString -> IO (S.Set URI)
getLinksFromPage uri body =
    -- convert to Set and remove duplicates
    return $ S.fromList filteredLinks
  where
    isOutgoing link = uriPath uri /= uriPath link
    sameDomain link = fromMaybe False (fromSameDomain uri link)

    tags = TS.parseTags body
    as = filter (TS.isTagOpenName "a") tags
    hrefs = map (TS.fromAttrib "href") as
    links = mapMaybe (transformToAbsolute uri) hrefs

    filteredLinks = filter (\link -> isOutgoing link && sameDomain link) links

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

-- | dump raw page bytes to file
dumpPageToFile :: URI -> BS.ByteString -> IO ()
dumpPageToFile pageUri body = do
  let (dirName, fileName) = uriToDirAndFileName pageUri
      outputDir = baseOutputDir </> dropDrive dirName
      outputFile = outputDir </> fileName
  createDirectoryIfMissing True outputDir
  BS.writeFile outputFile body

uriToDirAndFileName :: URI -> (FilePath, FilePath)
uriToDirAndFileName uri =
  let path = uriPath uri
      parts = splitOn "/" path
  in (intercalate "/" (init parts), last parts)
