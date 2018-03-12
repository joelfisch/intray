{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Intray.Server.Handler.ListItemsSpec
    ( spec
    ) where

import TestImport

import Intray.API
import Intray.Client

import Intray.Client.Gen ()
import Intray.Data.Gen ()
import Intray.Server.TestUtils

spec :: Spec
spec =
    withIntrayServer $
    describe "list items" $ do
        it "it lists items that were just added" $ \cenv ->
            forAllValid $ \items ->
                withValidNewUser cenv $ \token -> do
                    uuids <-
                        runClientOrError cenv $ mapM (clientAddItem token) items
                    items' <- runClientOrError cenv $ clientListItems token
                    map itemInfoIdentifier items' `shouldContain` uuids
        it "it always lists valid items" $ \cenv ->
            withValidNewUser cenv $ \token -> do
                items <- runClientOrError cenv $ clientListItems token
                shouldBeValid items
        it "does not list others' items" $ \cenv ->
            forAllValid $ \items1 ->
                forAllValid $ \items2 ->
                    withValidNewUser cenv $ \token1 ->
                        withValidNewUser cenv $ \token2 -> do
                            uuids1 <-
                                runClientOrError cenv $
                                mapM (clientAddItem token1) items1
                            uuids2 <-
                                runClientOrError cenv $
                                mapM (clientAddItem token2) items2
                            items' <-
                                runClientOrError cenv $ clientListItems token1
                            map itemInfoIdentifier items' `shouldContain` uuids1
                            forM_ (uuids2 :: [ItemUUID]) $ \u ->
                                u `shouldNotSatisfy`
                                (`elem` map itemInfoIdentifier items')
