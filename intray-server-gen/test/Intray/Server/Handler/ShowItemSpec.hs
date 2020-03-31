module Intray.Server.Handler.ShowItemSpec
  ( spec
  ) where

import TestImport

import Intray.Client

import Intray.API.Gen ()
import Intray.Server.TestUtils

spec :: Spec
spec =
  withIntrayServer $
  describe "GetShowItem" $ do
    it "fails without PermitShow" $ \cenv ->
      failsWithOutPermission cenv PermitShow $ \t -> clientGetShowItem t
    it "shows no item if the intray is empty, even if there are items in other accounts' intrays" $ \cenv ->
      forAllValid $ \t ->
        withValidNewUser cenv $ \t1 ->
          withValidNewUser cenv $ \t2 -> do
            mr <-
              runClientOrError cenv $ do
                void $ clientPostAddItem t1 t
                clientGetShowItem t2
            mr `shouldBe` Nothing
