{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.ShipsHarbors.Harbors where 

import Import

data Harbors = Harbors {harbors :: [Entity Harbor]}
    deriving Generic

instance FromJSON Harbors
instance ToJSON Harbors

data AHarbor = AHarbor {harbor :: Entity Harbor }
    deriving Generic

instance ToJSON AHarbor
instance FromJSON AHarbor


getAHarborR :: Text -> Handler Value
getAHarborR name = do
    harbor <- runDB $ getBy404 $ UniqueHarbor name
    returnJson $ AHarbor {harbor = harbor}

getHarborsR :: Handler Value 
getHarborsR = do 
   harbors <- runDB $ selectList [] []
   returnJson $ Harbors {harbors = harbors}

postHarborsR :: Handler Value
postHarborsR = do
    -- myTODO add better error handling in case trying to create a duplicate, currently its returning HTML of an error
    harbor :: Harbor <- requireCheckJsonBody
    newharbor <- runDB $ insert harbor
    sendResponseStatus status201 ("CREATED HARBOR" <> (harborName harbor) :: Text)

-- delete all is for development
deleteHarborsR :: Handler Value
deleteHarborsR = do 
    _ <- runDB $ deleteWhere ([] :: [Filter Harbor])
    sendResponseStatus status201 ("DELETED ALL HARBORS" :: String)
