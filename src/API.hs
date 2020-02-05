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

import           Servant.API
import           Servant.Client              ()
import           Servant.Server

import           Database                    (fetchPlayerPG)
import           Schema

type PlayersAPI =
       Capture "school" Text :> Capture "playername" Text :> Get '[JSON] [Entity Player]

playersAPI :: Proxy PlayersAPI
playersAPI = Proxy :: Proxy PlayersAPI

fetchPlayersHandler :: ConnectionString -> Text -> Text -> Handler [Entity Player]
fetchPlayersHandler connString name school = liftIO $ fetchPlayerPG connString school name

-- fetchPlayersYrHandler :: ConnectionString -> Text -> Text -> Handler Player
-- fetchPlayersYrHandler connString name yr = do
--   maybePlayer <- liftIO $ fetchPlayerYrPG connString name yr
--   case maybePlayer of
--     Just (Entity _ player) -> return player
--     Nothing -> Handler (throwE err401 { errBody = "Could not find user with that ID" })

playersServer :: ConnectionString -> Server PlayersAPI
playersServer connString =
  (fetchPlayersHandler connString)
  -- :<|> (fetchPlayersYrHandler connString)
