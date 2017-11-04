module Main where

import Lib

import Network.URI
import Options.Applicative
import Data.Semigroup ((<>))

parseUriOption :: ReadM URI
parseUriOption = eitherReader $
  \s ->
    case parseURIReference s of
      Just uri -> Right uri
      Nothing -> Left ("Invalid URI specified: " ++ s)

config :: Parser Configuration
config = Configuration <$>
            argument parseUriOption (
              metavar "START_URI"
              <> help "URI to start crawling from"
            ) <*>
            option auto (
              long "concurrency"
              <> short 'c'
              <> metavar "CONCURRENCY"
              <> help "concurrency level"
            ) <*>
            option auto (
              long "limit"
              <> short 'l'
              <> metavar "LIMIT"
              <> help "visited links limit"
            )

parserInfo = info config fullDesc

main :: IO ()
main = do
  cfg <- execParser parserInfo
  runCrawl cfg
