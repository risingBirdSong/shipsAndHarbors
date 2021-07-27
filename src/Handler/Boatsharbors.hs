{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Boatsharbors where


import Import
import Database.Esqueleto hiding (on, from)
import qualified Database.Esqueleto.Experimental as E

getAllShipsR :: Handler Import.Value
getAllShipsR = do
   ships :: [Entity Ship] <- runDB $ selectList [] []
   returnJson $ object $ [(pack "ships") .= ships ]

postNewShipR :: Handler Import.Value
postNewShipR = do 
    ship@Ship {..} <- requireCheckJsonBody :: Handler Ship 
    _ <- runDB $ insert ship
    sendResponseStatus status201 ("CREATED SHIP" :: String)

getAllHarborsR :: Handler Import.Value 
getAllHarborsR = do 
   harbors :: [Entity Harbor] <- runDB $ selectList [] []
   returnJson $ object $ [(pack "harbors") .= harbors]

postNewHarborR :: Handler Import.Value
postNewHarborR = do
    harbor@(Harbor {..}) <- requireCheckJsonBody
    _ <- runDB $ insert harbor
    sendResponseStatus status201 ("CREATED HARBOR" :: String)


postShipVisitHarborR :: Text -> Text -> Handler Import.Value 
postShipVisitHarborR shipname harborname = do 
   mShip <- runDB $ selectFirst [ShipName Import.==. shipname] [] 
   mHarbor <- runDB $ selectFirst [HarborName Import.==. harborname] []
   case (mShip, mHarbor) of
        (Just eship@(Entity sid ship), Just eharbor@(Entity hid harbor)) -> do
            _ <- runDB $ insert $ HarborShip (hid) sid
            returnJson $ object [(pack "ship" .= ship), (pack "harbor" .= harbor)]
        _ -> sendResponseStatus status201 ("NOT FOUND" :: String)


getShipsAtHarborR :: Text -> Handler Import.Value 
getShipsAtHarborR harborname = do 
    ships <- runDB $ select $ do 
        (ships E.:& harborship E.:& harbors) <- 
            E.from $ E.Table @Ship
            `E.InnerJoin` E.Table @HarborShip
            `E.on` (\(ship E.:& harborship) -> 
                ship ^. ShipId E.==. harborship ^. HarborShipShipId)
            `E.InnerJoin` E.Table @Harbor
            `E.on` (\ (_ E.:& harborship E.:& harbor) -> 
                harborship ^. HarborShipHarborId E.==. harbor ^. HarborId
                )
        where_ (harbors ^. HarborName E.==. val harborname)
        pure (ships)
    returnJson $ object $ [(pack "ships") .= ships]
