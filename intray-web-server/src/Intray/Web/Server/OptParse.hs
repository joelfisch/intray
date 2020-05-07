{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Intray.Web.Server.OptParse
  ( getInstructions
  , Instructions
  , Dispatch(..)
  , Settings(..)
  , ServeSettings(..)
  ) where

import qualified Data.Text as T
import Import
import qualified Intray.Server.OptParse as API
import Intray.Web.Server.OptParse.Types
import Options.Applicative
import qualified System.Environment as System
import Text.Read
import YamlParse.Applicative as YamlParse (confDesc, readConfigFile)

getInstructions :: IO Instructions
getInstructions = do
  (cmd, flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions cmd flags env config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags {..} Environment {..} mConf = do
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc func = mConf >>= func
  (API.DispatchServe apiSets, API.Settings) <-
    API.combineToInstructions
      (API.CommandServe serveFlagAPIFlags)
      flagAPIFlags
      envAPIEnvironment
      (confAPIConfiguration <$> mConf)
  let port = fromMaybe 8000 $ serveFlagPort <|> envPort <|> mc confPort
  when (API.serveSetPort apiSets == port) $
    die $
    unlines ["Web server port and API port must not be the same.", "They are both: " ++ show port]
  pure
    ( DispatchServe
        ServeSettings
          { serveSetAPISettings = apiSets
          , serveSetPort = port
          , serveSetPersistLogins =
              fromMaybe False (serveFlagPersistLogins <|> envPersistLogins <|> mc confPersistLogins)
          , serveSetTracking = serveFlagTracking <|> envTracking <|> mc confTracking
          , serveSetVerification = serveFlagVerification <|> envVerification <|> mc confVerification
          }
    , Settings)

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} = do
  configFile <-
    case API.flagConfigFile flagAPIFlags <|> API.envConfigFile envAPIEnvironment of
      Nothing -> API.getDefaultConfigFile
      Just cf -> resolveFile' cf
  YamlParse.readConfigFile configFile

getEnvironment :: IO Environment
getEnvironment = do
  env <- System.getEnvironment
  apiEnv <- API.getEnvironment
  let mv k = lookup ("INTRAY_WEB_SERVER_" <> k) env
      mr :: Read a => String -> Maybe a
      mr k = mv k >>= readMaybe
      mt = fmap T.pack . mv
  pure
    Environment
      { envAPIEnvironment = apiEnv
      , envPort = mr "PORT"
      , envPersistLogins = mr "PERSIST_LOGINS"
      , envTracking = mt "ANALYTICS_TRACKING_ID"
      , envVerification = mt "SEARCH_CONSOLE_VERIFICATION"
      }

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
    help_ = fullDesc <> progDesc description <> YamlParse.confDesc @Configuration
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
         (mconcat [long "web-port", metavar "PORT", value Nothing, help "the port to serve on"]) <*>
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
    modifier = fullDesc <> progDesc "Serve requests" <> YamlParse.confDesc @Configuration

parseFlags :: Parser Flags
parseFlags = Flags <$> API.parseFlags
