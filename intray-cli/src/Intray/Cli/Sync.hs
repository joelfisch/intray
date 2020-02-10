module Intray.Cli.Sync
  ( withClientStoreAndSync
  , modifyClientStoreAndSync
  , syncAndGet
  , syncAndReturn
  ) where

import Import

import Intray.Client

import Intray.Cli.Client
import Intray.Cli.OptParse
import Intray.Cli.Session
import Intray.Cli.Store

withClientStoreAndSync ::
     (ClientStore ItemUUID (AddedItem TypedItem) -> CliM (ClientStore ItemUUID (AddedItem TypedItem)))
  -> CliM ()
withClientStoreAndSync func = do
  before <- readClientStoreOrEmpty
  processed <- func before
  let req = makeSyncRequest processed
  strat <- asks setSyncStrategy
  mErrOrClientStore <-
    case strat of
      NeverSync -> pure Nothing
      AlwaysSync -> withToken $ \t -> runSingleClient $ clientPostSync t req
  after <-
    case mErrOrClientStore of
      Nothing -> pure processed
      Just errOrClientStore ->
        case errOrClientStore of
          Left err -> do
            liftIO $
              putStrLn $ unlines ["Sync failed, but store still modified succesfully:", show err]
            pure processed
          Right r -> pure $ mergeSyncResponse processed r
  writeClientStore after

modifyClientStoreAndSync ::
     (ClientStore ItemUUID (AddedItem TypedItem) -> ClientStore ItemUUID (AddedItem TypedItem))
  -> CliM ()
modifyClientStoreAndSync func = withClientStoreAndSync (pure . func)

syncAndGet :: (ClientStore ItemUUID (AddedItem TypedItem) -> CliM a) -> CliM a
syncAndGet func = do
  before <- readClientStoreOrEmpty
  let req = makeSyncRequest before
  strat <- asks setSyncStrategy
  mErrOrClientStore <-
    case strat of
      NeverSync -> pure Nothing
      AlwaysSync -> withToken $ \t -> runSingleClient $ clientPostSync t req
  case mErrOrClientStore of
    Nothing -> func before
    Just errOrClientStore ->
      case errOrClientStore of
        Left err -> do
          liftIO $ putStrLn $ unlines ["Sync failed, but still fetched succesfully:", show err]
          func before
        Right r -> do
          let after = mergeSyncResponse before r
          writeClientStore after
          func after

syncAndReturn :: (ClientStore ItemUUID (AddedItem TypedItem) -> a) -> CliM a
syncAndReturn func = syncAndGet $ pure . func
