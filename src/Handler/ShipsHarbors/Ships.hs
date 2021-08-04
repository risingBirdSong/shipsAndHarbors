{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}


module Handler.ShipsHarbors.Ships where 

import Import
import Handler.Utils


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
   theships <- runDB $ do 
      selected <- selectList [] []
      return selected
   returnJson $ Ships {ships = theships} 


getShipR :: Text -> Handler Value
getShipR name = do
    gotten <- runDB $ getBy404 $ UniqueShip name 
    returnJson $ AShip {ship = gotten}

-- myTODO make this Text a maybe?
postShipR :: Text-> Handler Value
postShipR todo = do 
    ship <- requireCheckJsonBody :: Handler Ship 
    -- myTODO add better error handling in case trying to create a duplicate, currently its returning HTML of an error
    _ <- runDB $ insert ship
    sendResponseStatus status201 ("CREATED SHIP" <> (shipName ship))

deleteShipR :: Text -> Handler Value
deleteShipR name = do
    (Entity sid ship) :: Entity Ship <- runDB $ getBy404OrErr (UniqueShip name)  ("That Ship was not found, nothing deleted" :: Text)          
    _ <- runDB $ deleteCascade sid 
    sendResponseStatus status201 ("DELETED" :: Text)

deleteAllShipsR :: Handler Value
deleteAllShipsR = do
    _ <- runDB $ deleteWhere ([] :: [Filter Ship])
    sendResponseStatus status201 ("DELETED ALL SHIPS" :: String)

