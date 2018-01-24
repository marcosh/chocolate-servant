module Main where

import           Config                      (Environment (..), makePool)
import           Database.Persist.Postgresql (runSqlPool)
import           Models                      (doMigrations)
import           Safe                        (readMay)
import           System.Environment          (lookupEnv)

main :: IO ()
main = do
    env <- lookupSetting "ENV" Development
    pool <- makePool env
    runSqlPool doMigrations pool

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
    where
        handleFailedRead str =
            error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]
