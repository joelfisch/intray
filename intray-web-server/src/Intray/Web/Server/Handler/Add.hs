{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Intray.Web.Server.Handler.Add
  ( getAddR
  , postAddR
  ) where

import Import

import Yesod

import qualified Network.HTTP.Types as Http
import Servant.Client

import Intray.Client

import Intray.Web.Server.Foundation

getAddR :: Handler Html
getAddR =
  withLogin $ \_ -> do
    token <- genToken
    withNavBar $(widgetFile "add")

newtype NewItem =
  NewItem
    { newItemText :: Textarea
    }

newItemForm :: FormInput Handler NewItem
newItemForm = NewItem <$> ireq textareaField "contents"

postAddR :: Handler Html
postAddR =
  withLogin $ \t -> do
    NewItem {..} <- runInputPost newItemForm
    errOrRes <- runClient $ clientPostAddItem t $ textTypedItem $ unTextarea newItemText
    case errOrRes of
      Left err ->
        handleStandardServantErrs err $ \resp ->
          case responseStatusCode resp of
            c
              | c == Http.unauthorized401 -> addNegativeMessage "You are not allowed to add items."
              | c == Http.paymentRequired402 ->
                addNegativeMessage
                  "You have reached the limit of the free plan, subscribe to be able to add more items. Click 'Account' to get started."
              | otherwise -> sendResponseStatus Http.status500 $ show resp
      Right _ -> pure ()
    redirect AddR
