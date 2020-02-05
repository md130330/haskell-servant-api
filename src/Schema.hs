{-# LANGUAGE DeriveGeneric              #-}
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

import           Data.Aeson           (ToJSON, object, toJSON, (.=))
import           Data.Text            (Text)
import           Database.Persist     (Entity (..))
import qualified Database.Persist.TH  as PTH

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistUpperCase|
  Player sql=player_stats
    name Text
    gamesPlayed Int Maybe
    gamesStarted Int Maybe
    minutesPerGame Double Maybe
    fieldGoalsMadePerGame Double Maybe
    fieldGoalsAttemptedPerGame Double Maybe
    fieldGoalPercentage Double Maybe
    twoPointsMadePerGame Double Maybe
    twoPointsAttemptedPerGame Double Maybe
    twoPointPercentage Double Maybe
    threePointsMadePerGame Double Maybe
    threePointsAttemptedPerGame Double Maybe
    threePointPercentage Double Maybe
    freeThrowsMadePerGame Double Maybe
    freeThrowsAttemptedPerGame Double Maybe
    freeThrowPercentage Double Maybe
    oRebPerGame Double Maybe
    dRebPerGame Double Maybe
    totalRebPerGame Double Maybe
    assistsPerGame Double Maybe
    stealsPerGame Double Maybe
    blocksPerGame Double Maybe
    turnoversPerGame Double Maybe
    foulsPerGame Double Maybe
    pointsPerGame Double Maybe
    year Int
    school Text
    UniquePlayer name year school
    Primary name year school
    deriving Show Read
|]

instance ToJSON Player where
  toJSON player = object
    [ "name" .=  playerName player
    , "gamesPlayed" .= playerGamesPlayed player
    , "gamesStarted" .= playerGamesStarted player
    , "minutesPerGame" .= playerMinutesPerGame player
    , "fieldGoalsMadePerGame" .= playerFieldGoalsMadePerGame player
    , "fieldGoalsAttemptedPerGame" .= playerFieldGoalsAttemptedPerGame player
    , "fieldGoalPercentage" .= playerFieldGoalPercentage player
    , "twoPointsMadePerGame" .= playerTwoPointsMadePerGame player
    , "twoPointstAttemptedPerGame" .= playerTwoPointsAttemptedPerGame player
    , "twoPointPercentage" .= playerTwoPointPercentage player
    , "threePointsMadePerGame" .= playerThreePointsMadePerGame player
    , "threePointsAttemptedPerGame" .= playerThreePointsAttemptedPerGame player
    , "threePointPercentage" .= playerThreePointPercentage player
    , "freeThrowsMadePerGame" .= playerFreeThrowsMadePerGame player
    , "freeThrowsAttemptedPerGame" .= playerFreeThrowsAttemptedPerGame player
    , "freeThrowPercentage" .= playerFreeThrowPercentage player
    , "oRebPerGame" .= playerORebPerGame player
    , "dRebPerGame" .= playerDRebPerGame player
    , "totalRebPerGame" .= playerTotalRebPerGame player
    , "assistsPerGame".= playerAssistsPerGame player
    , "stealsPerGame" .= playerStealsPerGame player
    , "blocksPerGame" .= playerBlocksPerGame player
    , "turnoversPerGame" .= playerTurnoversPerGame player
    , "foulsPerGame" .= playerFoulsPerGame player
    , "pointsPerGame" .= playerPointsPerGame player
    ]

instance ToJSON (Entity Player) where
  toJSON (Entity _ player) = object
    [ "name" .=  playerName player
    , "gamesPlayed" .= playerGamesPlayed player
    , "gamesStarted" .= playerGamesStarted player
    , "minutesPerGame" .= playerMinutesPerGame player
    , "fieldGoalsMadePerGame" .= playerFieldGoalsMadePerGame player
    , "fieldGoalsAttemptedPerGame" .= playerFieldGoalsAttemptedPerGame player
    , "fieldGoalPercentage" .= playerFieldGoalPercentage player
    , "twoPointsMadePerGame" .= playerTwoPointsMadePerGame player
    , "twoPointstAttemptedPerGame" .= playerTwoPointsAttemptedPerGame player
    , "twoPointPercentage" .= playerTwoPointPercentage player
    , "threePointsMadePerGame" .= playerThreePointsMadePerGame player
    , "threePointsAttemptedPerGame" .= playerThreePointsAttemptedPerGame player
    , "threePointPercentage" .= playerThreePointPercentage player
    , "freeThrowsMadePerGame" .= playerFreeThrowsMadePerGame player
    , "freeThrowsAttemptedPerGame" .= playerFreeThrowsAttemptedPerGame player
    , "freeThrowPercentage" .= playerFreeThrowPercentage player
    , "oRebPerGame" .= playerORebPerGame player
    , "dRebPerGame" .= playerDRebPerGame player
    , "totalRebPerGame" .= playerTotalRebPerGame player
    , "assistsPerGame".= playerAssistsPerGame player
    , "stealsPerGame" .= playerStealsPerGame player
    , "blocksPerGame" .= playerBlocksPerGame player
    , "turnoversPerGame" .= playerTurnoversPerGame player
    , "foulsPerGame" .= playerFoulsPerGame player
    , "pointsPerGame" .= playerPointsPerGame player
    ]
