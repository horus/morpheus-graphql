{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Server
  ( gqlSocketApp
  , initGQLState
  , GQLState
  ) where

import           Control.Exception                      (finally)
import           Control.Monad                          (forever)
import           Data.Text                              (Text)
import           Network.WebSockets                     (ServerApp, acceptRequestWith, forkPingThread, pendingRequest,
                                                         receiveData, sendTextData)

-- MORPHEUS
import           Data.Morpheus.Resolve.Resolve          (RootResCon, streamResolver)
import           Data.Morpheus.Server.Apollo            (SubAction (..), acceptApolloSubProtocol, apolloFormat,
                                                         toApolloResponse)
import           Data.Morpheus.Server.ClientRegister    (GQLState, addClientSubscription, connectClient,
                                                         disconnectClient, initGQLState, publishUpdates,
                                                         removeClientSubscription)
import           Data.Morpheus.Types.Internal.Stream    (ResponseEvent (..), ResponseStream, closeStream)
import           Data.Morpheus.Types.Internal.WebSocket (GQLClient (..))
import           Data.Morpheus.Types.IO                 (GQLResponse (..))
import           Data.Morpheus.Types.Resolver           (GQLRootResolver (..))

handleSubscription :: Eq e => GQLClient IO e c -> GQLState IO e c -> Text -> ResponseStream IO e c GQLResponse -> IO ()
handleSubscription GQLClient {clientConnection, clientID} state sessionId stream = do
  (actions, response) <- closeStream stream
  case response of
    Data _   -> mapM_ execute actions
    Errors _ -> sendTextData clientConnection (toApolloResponse sessionId response)
  where
    execute (Publish pub)   = publishUpdates state pub
    execute (Subscribe sub) = addClientSubscription clientID sub sessionId state

-- | Wai WebSocket Server App for GraphQL subscriptions
gqlSocketApp :: RootResCon IO e c que mut sub => GQLRootResolver IO e c que mut sub -> GQLState IO e c -> ServerApp
gqlSocketApp gqlRoot state pending = do
  connection <- acceptRequestWith pending $ acceptApolloSubProtocol (pendingRequest pending)
  forkPingThread connection 30
  client <- connectClient connection state
  finally (queryHandler client) (disconnectClient client state)
  where
    queryHandler client = forever handleRequest
      where
        handleRequest = receiveData (clientConnection client) >>= resolveMessage . apolloFormat
          where
            resolveMessage (SubError x) = print x
            resolveMessage (AddSub sessionId request) =
              handleSubscription client state sessionId (streamResolver gqlRoot request)
            resolveMessage (RemoveSub sessionId) = removeClientSubscription (clientID client) sessionId state
