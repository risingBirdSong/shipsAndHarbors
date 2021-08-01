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

newtype AShip = AShip {ship :: Entity Ship}
    deriving (Generic)

instance FromJSON AShip
instance ToJSON AShip 

getAllShipsR :: Handler Value
getAllShipsR = do
   theships <- runDB $ selectList [] []
   returnJson $ Ships {ships = theships} 

postNewShipR :: Handler Value
postNewShipR = do 
    ship@Ship {..} <- requireCheckJsonBody :: Handler Ship 
    _ <- runDB $ insert ship
    sendResponseStatus status201 ("CREATED SHIP" :: String)

data Harbors = Harbors {harbors :: [Entity Harbor]}
    deriving Generic

instance FromJSON Harbors
instance ToJSON Harbors

data AHarbor = AHarbor {harbor :: Entity Harbor }
    deriving Generic

instance ToJSON AHarbor
instance FromJSON AHarbor

getAShipR :: Text -> Handler Value
getAShipR name = do
    -- myTODO getBy404 is sending back a ton of HTML, i'd rather have just a nice and simple json response. so make my own version of getBy404
    gotten <- runDB $ getBy404 $ UniqueShip name 
    returnJson $ AShip {ship = gotten}

getAHarbor :: Text -> Handler Value
getAHarbor name = do
    harbor <- runDB $ getBy404 $ UniqueHarbor name
    returnJson $ AHarbor {harbor = harbor}

getAllHarborsR :: Handler Value 
getAllHarborsR = do 
   harbors <- runDB $ selectList [] []
   returnJson $ Harbors {harbors = harbors}

postNewHarborR :: Handler Value
postNewHarborR = do
    -- can do these ways, top is favorite
    harbor :: Harbor <- requireCheckJsonBody
    -- harbor <- requireCheckJsonBody :: Handler Harbor
    _ <- runDB $ insert harbor
    sendResponseStatus status201 ("Created Thing" :: Text)

-- delete all is for development
deleteAllHarborsR :: Handler Value
deleteAllHarborsR = do 
    _ <- runDB $ deleteWhere ([] :: [Filter Harbor])
    sendResponseStatus status201 ("DELETED ALL HARBORS" :: String)

deleteShipR :: Text -> Handler Value
deleteShipR name = do
    -- myTODO replace with getBy404
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
    -- myTODO try to extract the following two lines into a helper function
    ship <- runDB $ getBy404 $ UniqueShip shipname
    harbor <- runDB $ getBy404 $ UniqueHarbor harborname
    _ <- runDB $ insert $ Docking (entityKey harbor) (entityKey ship)
    sendResponseStatus status201 ("The ship " <> "has arrived in the Harbor " :: Text)

postShipLeavesHarborR :: Text -> Text -> Handler ()
postShipLeavesHarborR shipname harborname = do
    (Entity sid ship) <- runDB $ getBy404 $ UniqueShip shipname
    (Entity hid harbor) <- runDB $ getBy404 $ UniqueHarbor harborname
    docking <- runDB $ getBy404 $ UniqueDocking hid sid
    deleted <- runDB $ delete (entityKey docking)
    -- print "deleted" prints unit ()
    sendResponseStatus status201 (shipName ship <> " has left " <> harborName harbor)
-- myAdvancedTODO make a travel function that will handle a ship leaving harbor, travelling (based on travel speed and the distance in coords) and placing the ship in harbor

getShipsAtHarborR :: Text -> Handler Value 
getShipsAtHarborR harborname = do 
    shipsatharbor <- runDB $ shipsAtHarborDB harborname
    returnJson $ Ships {ships = shipsatharbor} 
