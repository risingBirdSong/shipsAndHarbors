
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-} -- myTODO what exactly is this pragma doing? Error below \/

-- • Illegal polymorphic type:
--         forall (m :: * -> *).
--         MonadUnliftIO m =>
--         ReaderT SqlBackend m [Entity Ship]
--       Perhaps you intended to use RankNTypes
--     • In the expansion of type synonym ‘DB’
--       In the type signature: shipsAtHarborDB :: Text -> DB [Entity Ship]
--    |
-- 12 | shipsAtHarborDB :: Text -> DB [Entity Ship]
--    |                    ^^^^^^^^^^^^^^^^^^^^^^^^


module Handler.BoatsharborsEsqeuleto (shipsAtHarborDB) where 

import Database.Esqueleto hiding (on, from, (==.), Value, delete)
import Database.Esqueleto.Experimental
import Import hiding ((==.), on )

shipsAtHarborDB :: Text -> DB [Entity Ship]
shipsAtHarborDB harborname = do
    ships <- select $ do 
        (ships :& docking :& harbors) <- 
            from $ Table @Ship
            `InnerJoin` Table @Docking
            `on` (\(ship :& docking) -> 
                ship ^. ShipId ==. docking ^. DockingShipId)
            `InnerJoin` Table @Harbor
            `on` (\ (_ :& docking :& harbor) -> 
                docking ^. DockingHarborId ==. harbor ^. HarborId
                )
        where_ (harbors ^. HarborName ==. val harborname)
        pure (ships)
    pure ships