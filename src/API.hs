{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Except  (throwE)
import           Data.Proxy                  (Proxy (..))
import           Data.Text                   (Text)
import           Database.Persist            (Entity (..))
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp    (run)
import           Servant.API
import           Servant.Client              ()
import           Servant.Server

import           Database                    (fetchPlayerPG, fetchPlayerYrPG,
                                              fetchPostgresConnection)
import           Schema

type PlayersAPI =
       "players" :> Capture "playername" Text :> Get '[JSON] [Entity Player]
  :<|> "players" :> Capture "playername" Text :> Capture "playeryear" Text :> Get '[JSON] Player

playersAPI :: Proxy PlayersAPI
playersAPI = Proxy :: Proxy PlayersAPI

fetchPlayersHandler :: ConnectionString -> Text -> Handler [Entity Player]
fetchPlayersHandler connString name = liftIO $ fetchPlayerPG connString name

fetchPlayersYrHandler :: ConnectionString -> Text -> Text -> Handler Player
fetchPlayersYrHandler connString name yr = do
  maybePlayer <- liftIO $ fetchPlayerYrPG connString name yr
  case maybePlayer of
    Just (Entity playerid player) -> return player
    Nothing -> Handler (throwE err401 { errBody = "Could not find user with that ID" })

playersServer :: ConnectionString -> Server PlayersAPI
playersServer connString =
  (fetchPlayersHandler connString)
  :<|> (fetchPlayersYrHandler connString)

runServer :: IO ()
runServer = do
  connString <- fetchPostgresConnection
  run 8000 (serve playersAPI (playersServer connString))
