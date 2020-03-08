{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Commands.Sync
  ( sync
  ) where

import Import

import Intray.Client

import Intray.Cli.Client
import Intray.Cli.OptParse
import Intray.Cli.Session
import Intray.Cli.Store

sync :: CliM ()
sync = do
  before <- readClientStoreOrEmpty
  let req = makeSyncRequest before
  mErrOrClientStore <- withToken $ \t -> runSingleClient $ clientPostSync t req
  after <-
    case mErrOrClientStore of
      Nothing -> liftIO $ die "No server configured."
      Just errOrClientStore ->
        case errOrClientStore of
          Left err -> liftIO $ die $ unlines ["Sync failed:", show err]
          Right resp -> do
            liftIO $ putStr $ showMergeStats req resp
            pure $ mergeSyncResponse before resp
  writeClientStore after

showMergeStats :: SyncRequest i a -> SyncResponse i a -> String
showMergeStats SyncRequest {..} SyncResponse {..} =
  unlines
    [ unwords [show $ length syncResponseServerAdded, "added   remotely"]
    , unwords [show $ length syncResponseServerDeleted, "deleted remotely"]
    , unwords [show $ length syncResponseClientAdded, "added   locally"]
    , unwords [show $ length syncResponseClientDeleted, "deleted locally"]
    ]
