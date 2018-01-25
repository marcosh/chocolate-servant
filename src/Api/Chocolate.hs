{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Chocolate where

import           Config                      (AppT)
import           Control.Monad.Except        (MonadIO)
import           Database.Persist.Postgresql (Entity, selectList)
import           Models                      (Chocolate, runDb)
import           Servant

type ChocolateAPI = "chocolates" :> Get '[JSON] [Entity Chocolate]

-- | The server that runs the ChocolateAPI
chocolateServer :: MonadIO m => ServerT ChocolateAPI (AppT m)
chocolateServer = allChocolates

-- | Returns all chocolates in the database
allChocolates :: MonadIO m => AppT m [Entity Chocolate]
allChocolates =
    runDb (selectList [] [])
