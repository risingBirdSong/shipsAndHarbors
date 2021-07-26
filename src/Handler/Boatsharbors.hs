{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}


module Handler.Boatsharbors where

import Import

getAllShipsR :: Handler Value
getAllShipsR = do
   ships :: [Entity Ship] <- runDB $ selectList [] []
   returnJson $ object $ [(pack "ships") .= ships ]

postNewShipR :: Handler Value
postNewShipR = do 
    ship@Ship {..} <- requireCheckJsonBody :: Handler Ship 
    _ <- runDB $ insert ship
    sendResponseStatus status201 ("CREATED SHIP" :: String)

getAllHarborsR :: Handler Value 
getAllHarborsR = do 
   harbors :: [Entity Harbor] <- runDB $ selectList [] []
   returnJson $ object $ [(pack "harbors") .= harbors]

postNewHarborR :: Handler Value
postNewHarborR = do
    harbor@(Harbor {..}) <- requireCheckJsonBody
    _ <- runDB $ insert harbor
    sendResponseStatus status201 ("CREATED HARBOR" :: String)


postShipVisitHarborR :: Text -> Text -> Handler Value 
postShipVisitHarborR shipname harborname = do 
   mShip <- runDB $ selectFirst [ShipName ==. shipname] [] 
   mHarbor <- runDB $ selectFirst [HarborName ==. harborname] []
   case (mShip, mHarbor) of
        (Just eship@(Entity sid ship), Just eharbor@(Entity hid harbor)) -> do
            _ <- runDB $ insert $ HarborShip (hid) sid
            returnJson $ object [(pack "ship" .= ship), (pack "harbor" .= harbor)]
        _ -> sendResponseStatus status201 ("NOT FOUND" :: String)

-- how to do this? 
--    AllHarborShipsR
-- getHarborShipsR :: Handler Value
-- getHarborShipsR = do 
--     harborship :: [Entity HarborShip] <- runDB $  selectList [] []
--     returnJson $ object $ [(pack "harborship") .= harborship]
    -- harborship :: ()
--    let vals = fmap snd harborship
--    vals :: ()
    -- sendResponseStatus status201 ("TODO" :: String)