module Config where

import           Database.Persist.Postgresql (ConnectionPool)

data Config
    = Config
    { configPool :: ConnectionPool
    }
