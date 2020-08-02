module Intray.Cli.Sync
  ( withClientStoreAndSync
  , modifyClientStoreAndSync
  , anyUnsyncedWarning
  , syncAndGet
  , syncAndReturn
  ) where

import Import
import Intray.Cli.Client
import Intray.Cli.OptParse
import Intray.Cli.Session
import Intray.Cli.Store
import Intray.Client

withClientStoreAndSync :: (CS -> CliM CS) -> CliM ()
withClientStoreAndSync func =
  withClientStore $ \before -> do
    processed <- func before
    let req = makeSyncRequest processed
    strat <- asks setSyncStrategy
    mErrOrClientStore <-
      case strat of
        NeverSync -> pure Nothing
        AlwaysSync -> withToken $ \t -> runSingleClient $ clientPostSync t req
    case mErrOrClientStore of
      Nothing -> pure processed
      Just errOrClientStore ->
        case errOrClientStore of
          Left err -> do
            liftIO $
              putStrLn $ unlines ["Sync failed, but store still modified succesfully:", show err]
            pure processed
          Right r -> do
            let after = mergeSyncResponse processed r
            anyUnsyncedWarning after
            pure after

modifyClientStoreAndSync :: (CS -> CS) -> CliM ()
modifyClientStoreAndSync func = withClientStoreAndSync (pure . func)

syncAndGet :: (CS -> CliM a) -> CliM a
syncAndGet func =
  withClientStore' $ \before -> do
    let req = makeSyncRequest before
    strat <- asks setSyncStrategy
    mErrOrClientStore <-
      case strat of
        NeverSync -> pure Nothing
        AlwaysSync -> withToken $ \t -> runSingleClient $ clientPostSync t req
    case mErrOrClientStore of
      Nothing -> (,) <$> func before <*> pure before
      Just errOrClientStore ->
        case errOrClientStore of
          Left err -> do
            liftIO $ putStrLn $ unlines ["Sync failed, but still fetched succesfully:", show err]
            (,) <$> func before <*> pure before
          Right r -> do
            let after = mergeSyncResponse before r
            anyUnsyncedWarning after
            (,) <$> func after <*> pure after

anyUnsyncedWarning :: CS -> CliM ()
anyUnsyncedWarning after =
  when (anyUnsynced after) $
  liftIO $
  putStrLn $
  unlines
    [ "Not all added items were synchronized in the most recent synchronisation."
    , "This may have occurred if you have not subscribed with your sync server."
    , "If that is the case, please navigate to your sync server's web interface to subscribe."
    ]

syncAndReturn :: (CS -> a) -> CliM a
syncAndReturn func = syncAndGet $ pure . func
