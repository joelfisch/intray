{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.OptParse
  ( getInstructions
  , Instructions
  , Dispatch(..)
  , Settings(..)
  , ServeSettings(..)
  ) where

import Import

import qualified Data.Text as T
import qualified System.Environment as System
import Text.Read

import Options.Applicative

import qualified Intray.Server.OptParse as API

import Intray.Web.Server.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
  (cmd, flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration cmd flags
  combineToInstructions cmd flags env config

combineToInstructions :: Command -> Flags -> Environment -> Configuration -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags Environment {..} Configuration = do
  ((API.DispatchServe apiSets), API.Settings) <-
    API.combineToInstructions
      (API.CommandServe serveFlagAPIFlags)
      API.Flags
      envAPIEnvironment
      API.Configuration
  let port = fromMaybe 8000 $ serveFlagPort `mplus` envPort
  when (API.serveSetPort apiSets == port) $
    die $
    unlines ["Web server port and API port must not be the same.", "They are both: " ++ show port]
  pure
    ( DispatchServe
        ServeSettings
          { serveSetAPISettings = apiSets
          , serveSetPort = port
          , serveSetPersistLogins = fromMaybe False serveFlagPersistLogins
          , serveSetTracking = serveFlagTracking
          , serveSetVerification = serveFlagVerification
          }
    , Settings)

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getEnvironment :: IO Environment
getEnvironment = do
  env <- System.getEnvironment
  apiEnv <- API.getEnvironment
  let mv k = lookup ("INTRAY_WEB_SERVER_" <> k) env
  pure Environment {envAPIEnvironment = apiEnv, envPort = mv "PORT" >>= readMaybe}

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
      ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Intray web server"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "serve" parseCommandServe]

parseCommandServe :: ParserInfo Command
parseCommandServe = info parser modifier
  where
    parser =
      CommandServe <$>
      (ServeFlags <$> API.parseServeFlags <*>
       option
         (Just <$> auto)
         (mconcat [long "port", metavar "PORT", value Nothing, help "the port to serve on"]) <*>
       flag
         Nothing
         (Just True)
         (mconcat
            [ long "persist-logins"
            , help
                "Whether to persist logins accross restarts. This should not be used in production."
            ]) <*>
       option
         (Just . T.pack <$> str)
         (mconcat
            [ long "analytics-tracking-id"
            , value Nothing
            , metavar "TRACKING_ID"
            , help "The google analytics tracking ID"
            ]) <*>
       option
         (Just . T.pack <$> str)
         (mconcat
            [ long "search-console-verification"
            , value Nothing
            , metavar "VERIFICATION_TAG"
            , help "The contents of the google search console verification tag"
            ]))
    modifier = fullDesc <> progDesc "Serve."

parseFlags :: Parser Flags
parseFlags = pure Flags
