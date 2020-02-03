{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import           Control.Monad.Reader (runReaderT)
import           Database.Persist (selectFirst, selectList, (==.), Entity)
import           Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT)
import           Data.Text (Text)

import           Schema

localConnString :: ConnectionString
localConnString = "host=college-basketball-api.c5w88jwm35y8.us-east-2.rds.amazonaws.com port=5432 user=administrator dbname=postgres password=wHinIfJnmCjSAhSYFb8S"

-- This is IO since in a real application we'd want to configure it.
fetchPostgresConnection :: IO ConnectionString
fetchPostgresConnection = return localConnString

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

fetchPlayerPG :: ConnectionString -> Text -> IO [Entity Player]
fetchPlayerPG connString name = runAction connString (selectList [PlayerName ==. name] [])

fetchPlayerYrPG :: ConnectionString -> Text -> Text -> IO (Maybe (Entity Player))
fetchPlayerYrPG connString name yr = runAction connString (selectFirst [PlayerName ==. name, PlayerClass ==. yr] [])
