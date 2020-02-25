{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (NoLoggingT, runStdoutLoggingT)
import           Control.Monad.Trans.Reader   (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Text                    (Text, append, pack)
import           Data.Text.Encoding           (encodeUtf8)
import           Database.Persist             (Entity, selectList, (==.))
import           Database.Persist.Postgresql  (ConnectionString, SqlBackend,
                                               runSqlPersistMPool,
                                               withPostgresqlPool)

import           Schema
import           SecretsManager


zipText :: [Text] -> [Text] -> [Text]
zipText (a:as) (b:bs) = append a b : zipText as bs
zipText _ _           = []

fetchPostgresConnection :: IO ConnectionString
fetchPostgresConnection = do
  host <- getSecret "host"
  port <- getSecret "port"
  user <- getSecret "user"
  dbname <- getSecret "dbname"
  password <- getSecret "password"
  let env = map pack ["host=", " port=", " user=", " dbname=", " password="]
  let val = [host, port, user, dbname, password]
  let connString = encodeUtf8 $ foldl append "" (zipText env val)
  return connString

runAction :: ConnectionString -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a-> IO a
runAction connectionString action =
  runStdoutLoggingT $ withPostgresqlPool connectionString 8 $ \pool ->
    liftIO $ runSqlPersistMPool action pool

fetchPlayerPG :: ConnectionString -> Text -> Text -> IO [Entity Player]
fetchPlayerPG connString name school = runAction connString (selectList [PlayerName ==. name, PlayerSchool ==. school] [])