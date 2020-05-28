{-# LANGUAGE RecordWildCards #-}

module Intray.Web.Server.Handler.ImageItem
  ( getImageItemR
  ) where

import qualified Data.Text.Encoding as TE
import Import
import Intray.Client
import Intray.Web.Server.Foundation
import Yesod

getImageItemR :: ItemUUID -> Handler TypedContent
getImageItemR uuid =
  withLogin $ \t -> do
    ItemInfo {..} <- runClientOrErr $ clientGetItem t uuid
    let TypedItem {..} = itemInfoContents
    case itemType of
      ImageItem it -> respond (TE.encodeUtf8 $ renderImageType it) itemData
      _ -> notFound
