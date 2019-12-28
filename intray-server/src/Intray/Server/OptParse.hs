{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.OptParse
  ( module Intray.Server.OptParse
  , module Intray.Server.OptParse.Types
  ) where

import Import

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sqlite
import Looper

import qualified System.Environment as System

import Options.Applicative

import Web.Stripe.Client as Stripe
import Web.Stripe.Types as Stripe

import Intray.API
import Intray.Server.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
  (cmd, flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration cmd flags
  combineToInstructions cmd flags env config

combineToInstructions :: Command -> Flags -> Environment -> Configuration -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags Environment {..} Configuration = do
  let port = fromMaybe 8001 $ serveFlagPort <|> envPort
  let connInfo = mkSqliteConnectionInfo $ fromMaybe "intray.db" serveFlagDb
  admins <-
    forM serveFlagAdmins $ \s ->
      case parseUsername $ T.pack s of
        Nothing -> die $ unwords ["Invalid admin username:", s]
        Just u -> pure u
  mmSets <-
    do let plan = Stripe.PlanId . T.pack <$> (serveFlagStripePlan <|> envStripePlan)
       let config =
             (\sk ->
                StripeConfig
                  { Stripe.secretKey = StripeKey $ TE.encodeUtf8 $ T.pack sk
                  , stripeEndpoint = Nothing
                  }) <$>
             (serveFlagStripeSecretKey <|> envStripeSecretKey)
       let publicKey = T.pack <$> (serveFlagStripePublishableKey <|> envStripePublishableKey)
       let fetcherSets =
             deriveLooperSettings
               (seconds 0)
               (minutes 1)
               serveFlagLooperStripeEventsFetcher
               envLooperStripeEventsRetrier
               Nothing
       let retrierSets =
             deriveLooperSettings
               (seconds 30)
               (hours 24)
               serveFlagLooperStripeEventsRetrier
               envLooperStripeEventsFetcher
               Nothing
       pure $
         MonetisationSettings <$> plan <*> config <*> publicKey <*> pure fetcherSets <*>
         pure retrierSets
  pure
    ( DispatchServe
        ServeSettings
          { serveSetPort = port
          , serveSetConnectionInfo = connInfo
          , serveSetAdmins = admins
          , serveSetMonetisationSettings = mmSets
          }
    , Settings)

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getEnvironment :: IO Environment
getEnvironment = do
  env <- System.getEnvironment
  let mv k = lookup ("INTRAY_SERVER_" <> k) env
      le n = readLooperEnvironment "INTRAY_SERVER_LOOPER_" n env
  pure
    Environment
      { envPort = mv "PORT" >>= readMaybe
      , envStripePlan = mv "STRIPE_PLAN"
      , envStripeSecretKey = mv "STRIPE_SECRET_KEY"
      , envStripePublishableKey = mv "STRIPE_PUBLISHABLE_KEY"
      , envLooperStripeEventsFetcher = le "STRIPE_EVENTS_FETCHER"
      , envLooperStripeEventsRetrier = le "STRIPE_EVENTS_RETRIER"
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
    help_ = fullDesc <> progDesc description
    description = "Intray server"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "serve" parseCommandServe]

parseCommandServe :: ParserInfo Command
parseCommandServe = info parser modifier
  where
    parser = CommandServe <$> parseServeFlags
    modifier = fullDesc <> progDesc "Command example."

parseServeFlags :: Parser ServeFlags
parseServeFlags =
  ServeFlags <$>
  option
    (Just <$> auto)
    (mconcat [long "api-port", value Nothing, metavar "PORT", help "the port to serve on"]) <*>
  option
    (Just . T.pack <$> str)
    (mconcat
       [ long "database"
       , value Nothing
       , metavar "DATABASE_CONNECTION_STRING"
       , help "The sqlite connection string"
       ]) <*>
  many (strOption (mconcat [long "admin", metavar "USERNAME", help "An admin to use"])) <*>
  option
    (Just <$> str)
    (mconcat
       [ long "stripe-plan"
       , value Nothing
       , metavar "PLAN_ID"
       , help "The product pricing plan for stripe"
       ]) <*>
  option
    (Just <$> str)
    (mconcat
       [ long "stripe-secret-key"
       , value Nothing
       , metavar "SECRET_KEY"
       , help "The secret key for stripe"
       ]) <*>
  option
    (Just <$> str)
    (mconcat
       [ long "stripe-publishable-key"
       , value Nothing
       , metavar "PUBLISHABLE_KEY"
       , help "The publishable key for stripe"
       ]) <*>
  getLooperFlags "stripe-events-fetcher" <*>
  getLooperFlags "stripe-events-retrier"

parseFlags :: Parser Flags
parseFlags = pure Flags
