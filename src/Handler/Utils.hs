{-# LANGUAGE TypeFamilies #-}

module Handler.Utils where 
import Import

-- myTODO understand how this is working...
getBy404OrErr ::
  (PersistUniqueRead backend, PersistEntity record, MonadHandler m,
   ToTypedContent c,
   PersistEntityBackend record ~ BaseBackend backend) =>
  Unique record -> c -> ReaderT backend m (Entity record)
getBy404OrErr key err = do
    mres <- getBy key
    case mres of
        Nothing -> sendResponseStatus status201 err
        Just res -> return res