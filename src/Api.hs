{-# LANGUAGE TypeOperators #-}

module Api (app) where

import           Api.Chocolate        (ChocolateAPI, chocolateServer)
import           Config               (AppT (..), Config (..))
import           Control.Category     ((<<<), (>>>))
import           Control.Monad.Except
import           Servant              (Proxy (Proxy))
import           Servant.Server

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function. @NT 'Handler'@ is a natural transformation that
-- effectively specialises app base monad to IO
appToServer :: Config -> Server ChocolateAPI
appToServer config = enter (convertApp config >>> NT Handler) chocolateServer

-- | This function converts our @'AppT' m@ monad into the @ExceptT ServantErr
-- m@ monad that Servant's 'enter' function needs in order to run the
-- application. The ':~>' type is a natural transformation, or, in
-- non-category theory terms, a function that converts two type
-- constructors without looking at the values in the types.
convertApp :: Config -> AppT m :~> ExceptT ServantErr m
convertApp config = runReaderTNat config <<< NT runApp

type AppAPI = ChocolateAPI

appApi :: Proxy AppAPI
appApi = Proxy

-- | Finally, this function takes a configuration and runs our 'ChocolateAPI'
app :: Config -> Application
app config =
    serve appApi $ appToServer config
