module Main where

import           API                      (playersAPI, playersServer)
import           Database                 (fetchPostgresConnection)
import           Network.Wai.Handler.Warp (run)
import           Servant.Server

main :: IO ()
main = do
  connString <- fetchPostgresConnection
  run 8000 (serve playersAPI (playersServer connString))
