{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Intray.Cli.OptParse
  ( Instructions(..)
  , getInstructions
  , Settings(..)
  , SyncStrategy(..)
  , Dispatch(..)
  , RegisterSettings(..)
  , LoginSettings(..)
  , CliM
  ) where

import qualified Data.Text as T
import Import
import Intray.Cli.OptParse.Types
import Intray.Data
import Options.Applicative
import Servant.Client
import qualified System.Environment as System
import Text.Read (readMaybe)
import qualified YamlParse.Applicative as YamlParse

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments cmd Flags {..}) Environment {..} mConf =
  Instructions <$> getDispatch <*> getSettings
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f
    getSettings = do
      setBaseUrl <-
        case flagUrl <|> envUrl <|> mc configUrl of
          Nothing -> pure Nothing
          Just url -> Just <$> parseBaseUrl url
      setCacheDir <-
        case flagCacheDir <|> envCacheDir <|> mc configCacheDir of
          Nothing -> getXdgDir XdgCache (Just [reldir|intray|])
          Just d -> resolveDir' d
      setDataDir <-
        case flagDataDir <|> envDataDir <|> mc configDataDir of
          Nothing -> getXdgDir XdgData (Just [reldir|intray|])
          Just d -> resolveDir' d
      let setSyncStrategy =
            fromMaybe
              (case setBaseUrl of
                 Nothing -> NeverSync
                 Just _ -> AlwaysSync) $
            flagSyncStrategy <|> envSyncStrategy <|> mc configSyncStrategy
      pure Settings {..}
    getDispatch =
      case cmd of
        CommandRegister RegisterArgs {..} ->
          pure $
          DispatchRegister
            RegisterSettings
              { registerSetUsername =
                  (T.pack <$> (registerArgUsername <|> envUsername <|> mc configUsername)) >>=
                  parseUsername
              , registerSetPassword = T.pack <$> (registerArgPassword <|> envPassword)
              }
        CommandLogin LoginArgs {..} ->
          pure $
          DispatchLogin
            LoginSettings
              { loginSetUsername =
                  (T.pack <$> (loginArgUsername <|> envUsername <|> mc configUsername)) >>=
                  parseUsername
              , loginSetPassword =
                  T.pack <$> (loginArgPassword <|> envPassword <|> mc configPassword)
              }
        CommandPostPostAddItem ss -> pure $ DispatchPostPostAddItem $ T.unwords $ map T.pack ss
        CommandShowItem -> pure DispatchShowItem
        CommandDoneItem -> pure DispatchDoneItem
        CommandSize -> pure DispatchSize
        CommandReview -> pure DispatchReview
        CommandLogout -> pure DispatchLogout
        CommandSync -> pure DispatchSync

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFiles >>= YamlParse.readFirstConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      YamlParse.readConfigFile afp

defaultConfigFiles :: IO [Path Abs File]
defaultConfigFiles =
  sequence
    [ do xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|intray|])
         resolveFile xdgConfigDir "config.yaml"
    , do homeDir <- getHomeDir
         intrayDir <- resolveDir homeDir ".intray"
         resolveFile intrayDir "config.yaml"
    ]

getEnvironment :: IO Environment
getEnvironment = do
  env <- System.getEnvironment
  let ms key = lookup ("INTRAY_" <> key) env
      mr key =
        case ms key of
          Nothing -> pure Nothing
          Just s ->
            case readMaybe s of
              Nothing -> die $ "Un-read-able value: " <> s
              Just r -> pure $ Just r
  let envConfigFile = ms "CONFIG_FILE"
      envUrl = ms "URL"
      envCacheDir = ms "CACHE_DIR"
      envDataDir = ms "DATA_DIR"
  envSyncStrategy <- mr "SYNC_STRATEGY"
  let envUsername = ms "USERNAME"
  let envPassword = ms "PASSWORD"
  pure Environment {..}

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser

prefs_ :: ParserPrefs
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
    help_ = fullDesc <> YamlParse.confDesc @Configuration

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
  mconcat
    [ command "register" parseCommandRegister
    , command "login" parseCommandLogin
    , command "add" parseCommandPostPostAddItem
    , command "show" parseCommandShowItem
    , command "done" parseCommandDoneItem
    , command "size" parseCommandSize
    , command "review" parseCommandReview
    , command "logout" parseCommandLogout
    , command "sync" parseCommandSync
    ]

parseCommandRegister :: ParserInfo Command
parseCommandRegister = info parser modifier
  where
    modifier = fullDesc <> progDesc "Register user"
    parser =
      CommandRegister <$>
      (RegisterArgs <$>
       option
         (Just <$> str)
         (mconcat
            [long "username", help "The username to register", value Nothing, metavar "USERNAME"]) <*>
       option
         (Just <$> str)
         (mconcat
            [ long "password"
            , help "The password to register with. If absent, a prompt will ask for the password."
            , value Nothing
            , metavar "PASSWORD"
            ]))

parseCommandLogin :: ParserInfo Command
parseCommandLogin = info parser modifier
  where
    modifier = fullDesc <> progDesc "Login user"
    parser =
      CommandLogin <$>
      (LoginArgs <$>
       option
         (Just <$> str)
         (mconcat [long "username", help "The username to login", value Nothing, metavar "USERNAME"]) <*>
       option
         (Just <$> str)
         (mconcat
            [long "password", help "The password to login with", value Nothing, metavar "PASSWORD"]))

parseCommandPostPostAddItem :: ParserInfo Command
parseCommandPostPostAddItem = info parser modifier
  where
    modifier = fullDesc <> progDesc "Add an item"
    parser =
      CommandPostPostAddItem <$>
      some
        (strArgument (mconcat [help "Give the contents of the item to be added.", metavar "TEXT"]))

parseCommandShowItem :: ParserInfo Command
parseCommandShowItem = info parser modifier
  where
    modifier = fullDesc <> progDesc "Show one item."
    parser = pure CommandShowItem

parseCommandDoneItem :: ParserInfo Command
parseCommandDoneItem = info parser modifier
  where
    modifier = fullDesc <> progDesc "Mark that item as done."
    parser = pure CommandDoneItem

parseCommandSize :: ParserInfo Command
parseCommandSize = info parser modifier
  where
    modifier = fullDesc <> progDesc "Show the number of items in the intray."
    parser = pure CommandSize

parseCommandReview :: ParserInfo Command
parseCommandReview = info parser modifier
  where
    modifier = fullDesc <> progDesc "Start reviewing items one by one."
    parser = pure CommandReview

parseCommandLogout :: ParserInfo Command
parseCommandLogout = info parser modifier
  where
    modifier = fullDesc <> progDesc "Logout user"
    parser = pure CommandLogout

parseCommandSync :: ParserInfo Command
parseCommandSync = info parser modifier
  where
    modifier = fullDesc <> progDesc "Sync the local and remote intray"
    parser = pure CommandSync

parseFlags :: Parser Flags
parseFlags =
  Flags <$>
  option
    (Just <$> str)
    (mconcat
       [ long "config-file"
       , help "Give the path to an altenative config file"
       , value Nothing
       , metavar "FILEPATH"
       ]) <*>
  option
    (Just <$> str)
    (mconcat [long "url", help "The url of the server.", value Nothing, metavar "URL"]) <*>
  option
    (Just <$> str)
    (mconcat
       [ long "cache-dir"
       , help "The directory to use for caching"
       , value Nothing
       , metavar "FILEPATH"
       ]) <*>
  option
    (Just <$> str)
    (mconcat
       [long "data-dir", help "The directory to use for data", value Nothing, metavar "FILEPATH"]) <*>
  syncStrategyOpt

syncStrategyOpt :: Parser (Maybe SyncStrategy)
syncStrategyOpt =
  flag Nothing (Just NeverSync) (mconcat [long "no-sync", help "Do not try to sync."]) <|>
  flag Nothing (Just AlwaysSync) (mconcat [long "sync", help "Definitely try to sync."])
