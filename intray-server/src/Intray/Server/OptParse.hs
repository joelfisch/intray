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
    do let price = serveFlagPrice <|> envPrice
       let config =
             (\sk ->
                StripeConfig
                  { Stripe.secretKey = StripeKey $ TE.encodeUtf8 $ T.pack sk
                  , stripeEndpoint = Nothing
                  }) <$>
             (serveFlagStripeSecretKey <|> envStripeSecretKey)
       let publicKey = T.pack <$> (serveFlagStripePublishableKey <|> envStripePublishableKey)
       pure $ MonetisationSettings <$> price <*> config <*> publicKey
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
  pure
    Environment
      { envPort = mv "PORT" >>= readMaybe
      , envPrice = mv "PRICE" >>= readMaybe
      , envStripeSecretKey = mv "STRIPE_SECRET_KEY"
      , envStripePublishableKey = mv "STRIPE_PUBLISHABLE_KEY"
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
    (mconcat [long "port", value Nothing, metavar "PORT", help "the port to serve on"]) <*>
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
    ((Just . Stripe.Amount) <$> auto)
    (mconcat [long "price", value Nothing, metavar "PRICE", help "The price, in cents"]) <*>
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
       ])

parseFlags :: Parser Flags
parseFlags = pure Flags
