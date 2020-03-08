{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Intray.Server.Handler.PostSync
  ( servePostSync
  ) where

import Import

import qualified Data.Map as M
import Data.Map (Map)
import Data.Mergeless
import Data.Set (Set)
import qualified Data.Set as S
import Data.UUID.Typed
import Database.Persist

import Intray.API

import Intray.Server.Handler.Stripe
import Intray.Server.Handler.Utils
import Intray.Server.Item
import Intray.Server.Types

servePostSync ::
     AuthCookie
  -> SyncRequest ItemUUID (AddedItem TypedItem)
  -> IntrayHandler (SyncResponse ItemUUID (AddedItem TypedItem))
servePostSync AuthCookie {..} sr = do
  ps <- getUserPaidStatus authCookieUserUUID
  doSync ps authCookieUserUUID sr

doSync ::
     PaidStatus
  -> AccountUUID
  -> SyncRequest ItemUUID (AddedItem TypedItem)
  -> IntrayHandler (SyncResponse ItemUUID (AddedItem TypedItem))
doSync ps userId sr = do
  let serverSyncProcessorDeleteMany s = do
        runDb $ deleteWhere [IntrayItemUserId ==. userId, IntrayItemIdentifier <-. S.toList s]
        pure s -- Just assume that everything was deleted.
      serverSyncProcessorQueryNoLongerSynced :: Set ItemUUID -> IntrayHandler (Set ItemUUID)
      serverSyncProcessorQueryNoLongerSynced s = do
        items <-
          runDb $ selectList [IntrayItemUserId ==. userId, IntrayItemIdentifier <-. S.toList s] []
        let inSButNotInStore =
              s `S.difference` S.fromList (map (intrayItemIdentifier . entityVal) items)
        pure inSButNotInStore
      serverSyncProcessorQueryNewRemote ::
           Set ItemUUID -> IntrayHandler (Map ItemUUID (AddedItem TypedItem))
      serverSyncProcessorQueryNewRemote s =
        M.fromList . map (makeAdded . entityVal) <$>
        runDb (selectList [IntrayItemUserId ==. userId, IntrayItemIdentifier /<-. S.toList s] [])
      serverSyncProcessorInsertMany ::
           Map ClientId (AddedItem TypedItem) -> IntrayHandler (Map ClientId ItemUUID)
      serverSyncProcessorInsertMany m = do
        let mFunc =
              case ps of
                HasNotPaid i -> take i
                HasPaid _ -> id
                NoPaymentNecessary -> id
        let m' = mFunc $ M.toList m
        fmap M.fromList $
          forM m' $ \(cid, AddedItem {..}) -> do
            uuid <- nextRandomUUID
            let ii = makeIntrayItem userId uuid addedItemCreated addedItemContents
            runDb $ insert_ ii
            pure (cid, uuid)
      proc = ServerSyncProcessor {..}
  processServerSyncCustom proc sr
