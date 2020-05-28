{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Add
  ( getAddR
  , postAddR
  ) where

import Import
import Intray.Client
import Intray.Web.Server.Foundation
import qualified Network.HTTP.Types as Http
import Yesod

getAddR :: Handler Html
getAddR =
  withLogin $ \_ -> do
    token <- genToken
    withNavBar $(widgetFile "add")

newItemTextForm :: FormInput Handler Textarea
newItemTextForm = ireq textareaField "contents"

newItemImageForm :: FormInput Handler FileInfo
newItemImageForm = ireq fileField "image"

postAddR :: Handler Html
postAddR =
  withLogin $ \t -> do
    tfr <- runInputPostResult newItemTextForm
    let goOn errs = do
          ifr <- runInputPostResult newItemImageForm
          case ifr of
            FormFailure ts -> invalidArgs $ ts ++ errs
            FormMissing -> invalidArgs $ [] ++ errs
            FormSuccess fi -> do
              itemData <- fileSourceByteString fi
              itemType <-
                case parseImageType (fileContentType fi) of
                  Nothing -> invalidArgs ["Unsupported image type."]
                  Just typ -> pure $ ImageItem typ
              pure TypedItem {..}
    ti <-
      case tfr of
        FormSuccess ta -> pure $ textTypedItem $ unTextarea ta
        FormFailure ts -> goOn ts
        FormMissing -> goOn []
    errOrRes <- runClient $ clientPostAddItem t ti
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
