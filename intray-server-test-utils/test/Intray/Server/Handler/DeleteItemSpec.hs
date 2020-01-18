module Intray.Server.Handler.DeleteItemSpec
  ( spec
  ) where

import TestImport

import Network.HTTP.Types.Status
import Servant.Client

import Intray.Client

import Intray.API.Gen ()
import Intray.Server.TestUtils

spec :: Spec
spec =
  withIntrayServer $
  describe "DeleteItem" $ do
    it "fails without PermitDelete" $ \cenv ->
      forAllValid $ \uuid ->
        failsWithOutPermission cenv PermitDelete $ \t -> clientDeleteItem t uuid
    it "succesfully manages to delete the item that was just added" $ \cenv ->
      forAllValid $ \t ->
        withValidNewUser cenv $ \token -> do
          errOrItem <-
            runClient cenv $ do
              uuid <- clientPostAddItem token t
              void $ clientDeleteItem token uuid
              clientGetItem token uuid
          case errOrItem of
            Left err ->
              case err of
                FailureResponse resp -> statusCode (responseStatusCode resp) `shouldBe` 404
                _ -> expectationFailure $ unwords ["Unexpected error:", show err]
            Right _ -> expectationFailure "Should not have succeeded."
