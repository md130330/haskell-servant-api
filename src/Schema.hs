{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Schema where

import           Data.Aeson          (FromJSON, Object, ToJSON, object,
                                      parseJSON, toJSON, withObject, (.:), (.=))
import           Data.Aeson.Types    (Parser)
import           Data.Text           (Text)
import           Database.Persist    (Entity (..))
import qualified Database.Persist.TH as PTH

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Player sql=gonzaga
    name Text
    number Int Maybe
    class Text
    position Text
    height Text
    weight Int Maybe
    hometown Text Maybe
    highschool Text Maybe
    rsci Text Maybe Maybe
    summary Text
    Primary name
    deriving Show Read
|]

instance ToJSON Player where
  toJSON player = object
    [ "name" .= playerName player
    , "number" .= playerNumber player
    , "class" .= playerClass player
    , "position" .= playerPosition player
    , "height" .= playerHeight player
    , "weight" .= playerWeight player
    , "hometown" .= playerHometown player
    , "highschool" .= playerHighschool player
    , "rsci" .= playerRsci player
    , "summary" .= playerSummary player
    ]

instance ToJSON (Entity Player) where
  toJSON (Entity playerid player) = object
    [ "name" .= playerName player
    , "number" .= playerNumber player
    , "class" .= playerClass player
    , "position" .= playerPosition player
    , "height" .= playerHeight player
    , "weight" .= playerWeight player
    , "hometown" .= playerHometown player
    , "highschool" .= playerHighschool player
    , "rsci" .= playerRsci player
    , "summary" .= playerSummary player
    ]

instance FromJSON Player where
  parseJSON = withObject "Player" parseUser

parseUser :: Object -> Parser Player
parseUser o = do
  pName <- o .: "name"
  pNumber <- o .: "number"
  pClass <- o .: "class"
  pPosition <- o .: "position"
  pHeight <- o .: "height"
  pWeight <- o .: "weight"
  pHometown <- o .: "hometown"
  pHighschool <- o .: "highschool"
  pRsci <- o .: "rsci"
  pSummary <- o .: "summary"
  return Player
    { playerName = pName
    , playerNumber = pNumber
    , playerClass = pClass
    , playerPosition = pPosition
    , playerHeight = pHeight
    , playerWeight = pWeight
    , playerHometown = pHometown
    , playerHighschool = pHighschool
    , playerRsci = pRsci
    , playerSummary = pSummary
    }
