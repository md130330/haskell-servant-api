{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import           Control.Monad.IO.Class      (liftIO)
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

playersServer :: ConnectionString -> Server PlayersAPI
playersServer connString =
  (fetchPlayersHandler connString)
