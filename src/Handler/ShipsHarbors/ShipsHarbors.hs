{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}


module Handler.ShipsHarbors.ShipsHarbors where

import Import
import Database.Esqueleto hiding (on, from, (==.), Value, delete)
import qualified Database.Esqueleto.Experimental as E
import Handler.ShipsHarbors.BoatsharborsEsqeuleto
import Handler.Utils
import Handler.ShipsHarbors.Ships (Ships(..))
import Handler.ShipsHarbors.Harbors (Harbors(..))

import Data.Aeson


postShipVisitHarborR :: Text -> Text -> Handler Value 
postShipVisitHarborR shipname harborname = do 
    (ship, harbor) <- shipAndHarborGet shipname harborname
    _ <- runDB $ insert $ Docking (entityKey harbor) (entityKey ship)
    sendResponseStatus status201 ("The ship " <> (shipName $ entityVal ship)  <> " has arrived in the Harbor " <> (harborName $ entityVal harbor) :: Text)

postShipLeavesHarborR :: Text -> Text -> Handler ()
postShipLeavesHarborR shipname harborname = do
    (Entity sid ship, Entity hid harbor) <- shipAndHarborGet shipname harborname
    docking <- runDB $ getBy404 $ UniqueDocking hid sid
    deleted <- runDB $ delete (entityKey docking)
    sendResponseStatus status201 (shipName ship <> " has left " <> harborName harbor)
-- myAdvancedTODO make a travel function that will handle a ship leaving harbor, travelling (based on travel speed and the distance in coords) and placing the ship in harbor

getShipsAtHarborR :: Text -> Handler Value 
getShipsAtHarborR harborname = do 
    shipsatharbor <- runDB $ shipsAtHarborDB harborname
    returnJson $ Ships {ships = shipsatharbor} 

-- helper functions

shipAndHarborGet :: Text -> Text -> Handler (Entity Ship, Entity Harbor)
shipAndHarborGet shipname harborname = do
    ship <- runDB $ getBy404 $ UniqueShip shipname
    harbor <- runDB $ getBy404 $ UniqueHarbor harborname
    return (ship, harbor) 


-- Experimental stuff

-- maybetest
postMaybetestR :: Maybe Text -> Handler ()
postMaybetestR mText = do 
    case mText of
        Nothing -> print "got nothing"
        Just t -> print t
    return ()

-- interesting, that this how the Maybe type works. It's literally a Maybe type. I thought it'd be like either just the url string or leaving it blank

--  http://localhost:3000/maybetest/Nothing
-- "got nothing"

--  http://localhost:3000/maybetest/Just somedata
-- "somedata"



getQuerysqlR :: Handler ()
getQuerysqlR = do
   a <- runDB $ rendered "shipaaa"
   print a
   return ()