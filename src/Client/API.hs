{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Client.API
  ( listSessionsClient
  , getSessionClient
  , listTopicsClient
  , createTopicClient
  , topicClient
  , TopicClient(..)
  , TopicEventClient(..)
  ) where

import Data.Aeson
import Data.UUID
import Servant
import Servant.Client
import qualified Data.Aeson.KeyMap as KM

import API
import API.Helpers
import Event


type GetSessionClientM = UUID -> ClientM SessionResponse


type ListSessionsClientM = ClientM SessionsResponse


type SessionsClientM = ListSessionsClientM :<|> GetSessionClientM


type CreateTopicClientM = CreateTopic -> ClientM TopicResponse


type ListTopicsClientM = ClientM TopicsResponse


type GetTopicClientM = ClientM TopicResponse


type DeleteTopicClientM = ClientM NoContent


type ListTopicEventsClientM = Maybe Int -> Maybe Int -> ClientM (ListResponse EventData)


type GetTopicEventClientM = ClientM EventData


type DeleteTopicEventClientM = ClientM NoContent


type UpdateTopicEventClientM = KM.KeyMap Value -> ClientM EventData


type TopicEventClientM =
  GetTopicEventClientM :<|> DeleteTopicEventClientM :<|> UpdateTopicEventClientM


type TopicEventsClientM = ListTopicEventsClientM :<|> (UUID -> TopicEventClientM)


type TopicClientM
  = GetTopicClientM
  :<|> DeleteTopicClientM
  :<|> (UpdateTopic -> ClientM TopicResponse)
  :<|> TopicEventsClientM


type TopicsClientM = ListTopicsClientM :<|> CreateTopicClientM :<|> (UUID -> TopicClientM)


data TopicClient = TopicClient
  { getTopicClient :: GetTopicClientM
  , deleteTopicClient :: DeleteTopicClientM
  , updateTopicClient :: UpdateTopic -> ClientM TopicResponse
  , listTopicEventsClient :: ListTopicEventsClientM
  , topicEventClient :: UUID -> TopicEventClient
  }


data TopicEventClient = TopicEventClient
  { getTopicEventClient :: GetTopicEventClientM
  , deleteTopicEventClient :: DeleteTopicEventClientM
  , updateTopicEventClient :: KM.KeyMap Value -> ClientM EventData
  }


sessionsClient :: SessionsClientM
topicsClient :: TopicsClientM
sessionsClient :<|> topicsClient = client (Proxy :: Proxy ProtectedAPI)


getSessionClient :: GetSessionClientM
listSessionsClient :: ListSessionsClientM


listSessionsClient :<|> getSessionClient = sessionsClient


listTopicsClient :: ListTopicsClientM
createTopicClient :: CreateTopicClientM
mkTopicClient :: UUID -> TopicClientM
listTopicsClient :<|> createTopicClient :<|> mkTopicClient = topicsClient


topicClient :: UUID -> TopicClient
topicClient topicId =
  let
    getTopicClient' :<|>
      deleteTopicClient' :<|>
      updateTopicClient' :<|>
      (listTopicEventsClient' :<|> mkTopicEventClient') = mkTopicClient topicId
    mkTopicEventClient :: UUID -> TopicEventClient
    mkTopicEventClient eventId =
      let
        getTopicEventClient' :<|>
          deleteTopicEventClient' :<|>
          updateTopicEventClient' = mkTopicEventClient' eventId
      in
        TopicEventClient
          { getTopicEventClient = getTopicEventClient'
          , deleteTopicEventClient = deleteTopicEventClient'
          , updateTopicEventClient = updateTopicEventClient'
          }
  in
    TopicClient
      { getTopicClient = getTopicClient'
      , deleteTopicClient = deleteTopicClient'
      , updateTopicClient = updateTopicClient'
      , listTopicEventsClient = listTopicEventsClient'
      , topicEventClient = mkTopicEventClient
      }
