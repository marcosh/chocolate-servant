{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Config where

import           Control.Exception           (throwIO)
import           Control.Monad.Except        (ExceptT, MonadError)
import           Control.Monad.Logger        (MonadLogger, monadLoggerLog,
                                              toLogStr)
import           Control.Monad.Reader        (MonadIO, MonadReader, ReaderT,
                                              liftIO)
import           Control.Monad.Trans         (lift)
import           Control.Monad.Trans.Maybe   (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8       as BS
import           Data.Monoid                 ((<>))
import           Database.Persist.Postgresql (ConnectionPool, ConnectionString,
                                              createPostgresqlPool)
import           Servant                     (ServantErr)
import           System.Environment          (lookupEnv)
import           System.Log.FastLogger       (fromLogStr)

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype AppT m a
    = AppT
    { runApp :: ReaderT Config (ExceptT ServantErr m) a
    } deriving ( Functor, Applicative, Monad, MonadReader Config,
                 MonadError ServantErr, MonadIO)

data Config
    = Config
    { configPool :: ConnectionPool
    }

instance (Monad m, MonadIO m) => MonadLogger m where
    monadLoggerLog _ _ _ msg =
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
makePool Development =
    createPostgresqlPool (connectionString "dev") (envPool Development)
makePool Test =
    createPostgresqlPool (connectionString "test") (envPool Test)

envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

connectionString :: BS.ByteString -> ConnectionString
connectionString dbName = "host=localhost dbname=" <> dbName <> " user=user password=password port=5432"
