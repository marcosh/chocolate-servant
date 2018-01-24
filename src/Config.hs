{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Config where

import           Control.Exception           (throwIO)
import           Control.Monad.Logger        (MonadLogger, monadLoggerLog,
                                              toLogStr)
import           Control.Monad.Reader        (MonadIO, liftIO)
import           Control.Monad.Trans         (lift)
import           Control.Monad.Trans.Maybe   (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8       as BS
import           Data.Monoid                 ((<>))
import           Database.Persist.Postgresql (ConnectionPool,
                                              createPostgresqlPool)
import           System.Environment          (lookupEnv)
import           System.Log.FastLogger       (fromLogStr)

data Config
    = Config
    { configPool :: ConnectionPool
    }

instance (Monad m, MonadIO m) => MonadLogger m where
    monadLoggerLog loc source level msg =
        liftIO $  putStrLn $ BS.unpack $ (fromLogStr . toLogStr) msg

data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

makePool :: Environment -> IO ConnectionPool
makePool Production = do
    pool <- runMaybeT $ do
        let keys =
                [ "host="
                , "port="
                , "user="
                , "password"
                , "dbname"
                ]
            envs =
                [ "PGHOST"
                , "PGPORT"
                , "PGUSER"
                , "PGPASS"
                , "PGDATABASE"
                ]
        envVars <- traverse (MaybeT . lookupEnv) envs
        let prodStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
        lift $ createPostgresqlPool prodStr (envPool Production)
    case pool of
        Nothing -> throwIO (userError "Database configuration not present in environment")
        Just a -> return a


envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8
