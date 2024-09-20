{-# LANGUAGE OverloadedStrings #-}
module Events.Client
  ( loadEventsClientConfig
  , newEventsClient
  , runEventsClient
  , EventsClient(..)
  ) where

import Control.Concurrent.STM (readTVarIO)
import Data.Text (pack)
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.Client
import System.Environment (lookupEnv)

import IAM.Client
import IAM.Client.Auth


data EventsClient = EventsClient
  { eventsClientConfig :: EventsClientConfig
  , eventsClientManager :: Manager
  , eventsClientIAM :: IAMClient
  , eventsClientEnv :: ClientEnv
  }


-- | Client configuration
newtype EventsClientConfig = EventsClientConfig
  { eventsClientConfigBaseUrl :: String
  }


-- | load client configuration from environment
loadEventsClientConfig :: IO EventsClientConfig
loadEventsClientConfig = do
  url <- lookupEnv "MTAYLOR_IO_EVENTS_URL"
  case url of
    Just url' -> return $ EventsClientConfig url'
    Nothing -> return $ EventsClientConfig "https://events.mtaylor.io"


-- | create a new client
newEventsClient :: EventsClientConfig -> IAMClient -> IO EventsClient
newEventsClient config iam = do
  let r = requestAuth iam
  mgr <- newManager tlsManagerSettings { managerModifyRequest = r }
  url <- parseBaseUrl (eventsClientConfigBaseUrl config)
  return $ EventsClient config mgr iam $ mkClientEnv mgr url


-- | request authentication signatures
requestAuth :: IAMClient -> Request -> IO Request
requestAuth iam req =
  case lookup "Authorization" (requestHeaders req) of
    Just _ -> return req
    Nothing -> do
      requestId <- nextRandom
      maybeSessionToken <- readTVarIO (iamClientSessionTokenVar iam)
      let iamConfig = iamClientConfig iam
      let user = pack $ iamClientConfigUserIdentifier iamConfig
      let publicKey = encodePublicKey $ iamClientConfigSecretKey iamConfig
      let stringToSign = authStringToSign req requestId maybeSessionToken
      let authorization = authHeader stringToSign $ iamClientConfigSecretKey iamConfig
      return $ req
        { requestHeaders = requestHeaders req ++
          authReqHeaders authorization user publicKey requestId maybeSessionToken
        }


runEventsClient :: EventsClient -> ClientM a -> IO (Either ClientError a)
runEventsClient c a = runClientM a (eventsClientEnv c)
