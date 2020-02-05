{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module SecretsManager where

import           Control.Lens.Operators ((&), (.~), (^.))
import           Control.Monad          (void)
import qualified Data.List.NonEmpty     as NonEmpty (fromList)
import           Data.Maybe             (fromJust)
import           Data.Text              (Text)
import           Network.AWS            (Credentials (..), Region (..), send)
import           Network.AWS.Easy       (Endpoint (..), awsConfig,
                                         awscCredentials, connect, withAWS,
                                         wrapAWSService)
import           SSMImports
import           System.Directory       (getHomeDirectory)
import           System.FilePath        ((</>))

wrapAWSService 'ssm "SSMService" "SSMSession"

newtype ParameterName = ParameterName Text

newtype ParameterValue = ParameterValue Text

doPutParameter :: ParameterName -> ParameterValue -> SSMSession -> IO ()
doPutParameter (ParameterName pn) (ParameterValue pv) = withAWS $
    void (send $ putParameter pn pv String & ppOverwrite .~ Just True)

doGetParameter :: ParameterName -> SSMSession -> IO (Text, Integer)
doGetParameter (ParameterName pn) = withAWS $ do
    result <- send $ getParameters (NonEmpty.fromList [pn])
    let param = head $ result ^. grsParameters
    return $ (fromJust (param ^. pValue), fromJust (param ^. pVersion))


getSsmSession :: IO SSMSession
getSsmSession = do
  homeDir <- getHomeDirectory
  let conf = awsConfig (AWSRegion Ohio) & awscCredentials .~ (FromFile "default" $ homeDir </> ".aws" </> "credentials")
  ssmSession <- connect conf ssmService
  return ssmSession

getSecret :: Text -> IO Text
getSecret parameterName = do
    ssmSession <- getSsmSession
    (value, _) <- doGetParameter (ParameterName parameterName) ssmSession
    return value

setSecret :: Text -> Text -> IO ()
setSecret parameterName parameterValue = do
    ssmSession <- getSsmSession
    doPutParameter (ParameterName parameterName) (ParameterValue parameterValue) ssmSession

