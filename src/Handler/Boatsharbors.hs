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


module Handler.Boatsharbors where


import Import
import Database.Esqueleto hiding (on, from, (==.), Value, delete)
import qualified Database.Esqueleto.Experimental as E
import Handler.BoatsharborsEsqeuleto (shipsAtHarborDB)

import qualified Data.Aeson as J


newtype Ships = Ships {ships :: [Entity Ship]}
    deriving (Generic)

instance FromJSON Ships
instance ToJSON Ships

getAllShipsR :: Handler Value
getAllShipsR = do
   theships <- runDB $ selectList [] []
   returnJson $ Ships {ships = theships} 


postNewShipR :: Handler Value
postNewShipR = do 
    ship@Ship {..} <- requireCheckJsonBody :: Handler Ship 
    _ <- runDB $ insert ship
    sendResponseStatus status201 ("CREATED SHIP" :: String)

data GetAllHarbors = GetAllHarbors {harbors :: [Entity Harbor]}
    deriving Generic

instance FromJSON GetAllHarbors
instance ToJSON GetAllHarbors

getAllHarborsR :: Handler Value 
getAllHarborsR = do 
   harbors<- runDB $ selectList [] []
   returnJson $ GetAllHarbors {harbors = harbors}

postNewHarborR :: Handler Value
postNewHarborR = do
    harbor@(Harbor {..}) <- requireCheckJsonBody
    _ <- runDB $ insert harbor
    sendResponseStatus status201 ("CREATED HARBOR" :: String)

-- delete all is for development
deleteAllHarborsR :: Handler Value
deleteAllHarborsR = do 
    _ <- runDB $ deleteWhere ([] :: [Filter Harbor])
    sendResponseStatus status201 ("DELETED ALL HARBORS" :: String)

deleteShipR :: Text -> Handler Value
deleteShipR name = do
    meShip <- runDB $ selectFirst [ShipName ==. name] []
    case meShip of
        Nothing -> sendResponseStatus status201 ("Ship not found, not deleted" :: String) 
        Just (Entity sid ship) -> (runDB $ deleteCascade sid) >> sendResponseStatus status201 ("DELETED" :: String)
    
deleteAllShipsR :: Handler Value
deleteAllShipsR = do
    _ <- runDB $ deleteWhere ([] :: [Filter Ship])
    sendResponseStatus status201 ("DELETED ALL SHIPS" :: String)

postShipVisitHarborR :: Text -> Text -> Handler Value 
postShipVisitHarborR shipname harborname = do 
   mShip <- runDB $ selectFirst [ShipName ==. shipname] [] 
   mHarbor <- runDB $ selectFirst [HarborName ==. harborname] []
   case (mShip, mHarbor) of
        (Just (Entity sid ship), Just (Entity hid harbor)) -> do
            _ <- runDB $ insert $ Docking (hid) sid
            sendResponseStatus status201 ("The ship " <> (shipName ship) <> "has arrived in the Harbor " <> (harborName harbor) :: Text)
        _ -> sendResponseStatus status201 ("NOT FOUND" :: String)

postShipLeavesHarborR :: Text -> Text -> Handler Value
postShipLeavesHarborR shipname harborname = do
   mHarbor <- runDB $ selectFirst [HarborName ==. harborname] []
   mShip <- runDB $ selectFirst [ShipName ==. shipname] []
   case (mHarbor, mShip) of 
        (Just (Entity hid harbor), Just (Entity sid ship)) -> do
            mDocking <- runDB $ selectFirst ([DockingHarborId ==. hid] ++ [DockingShipId ==. sid]) []
            case mDocking of
                (Just (Entity dId docking)) -> do
                    _ <- runDB $ delete dId 
                    sendResponseStatus status201 ((shipName ship ) <> " has left the harbor " <> (harborName harbor) :: Text) 
                Nothing -> sendResponseStatus status201 ("That ship was not at the harbor" :: String)   
        _ -> sendResponseStatus status201 ("Either the ship or the harbor was not found" :: String) 
    

getShipsAtHarborR :: Text -> Handler Value 
getShipsAtHarborR harborname = do 
    shipsatharbor <- runDB $ shipsAtHarborDB harborname
    returnJson $ Ships {ships = shipsatharbor} 
