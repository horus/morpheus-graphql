{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main
  ( main
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class         (liftIO)
import           Data.Aeson
import           Data.ByteString.Lazy           (ByteString)
import           Data.Morpheus                  (Interpreter (..))
import           Data.Morpheus.Client           (Fetch (..), defineQuery, gql)
import           Data.Morpheus.Document         (toGraphQLDocument)
import           Data.Morpheus.Server           (GQLState, gqlSocketApp, initGQLState)
import           Data.Proxy                     (Proxy (..))
import           Deprecated.API                 (Channel, Content, gqlRoot)
import           GHC.Generics
import           Mythology.API                  (mythologyApi)
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import           Network.WebSockets             (defaultConnectionOptions)
import           Web.Scotty                     (body, file, get, post, raw, scottyApp)

type Lifetime = String

type Power = String

defineQuery
  [gql|
    query GetHero {
      deity {
         power
         fullName
        }
      hero {
         lifetime
      }
    }
  |]

jsonRes :: String -> IO ByteString
jsonRes _ =
  return "{\"deity\":{ \"power\":\"Power\",  \"fullName\":\"name\" }, \"hero\":{ \"lifetime\":\"Lifetime\"}  }"

getHero :: GetHero
getHero = GetHero {deity = Deity {power = "", fullName = "ass"}, hero = Human {lifetime = ""}}

fetchHero :: IO (Either String GetHero)
fetchHero = fetch jsonRes

myQuery :: String
myQuery = queryFor (Proxy :: Proxy GetHero)

main :: IO ()
main = do
  print myQuery
  print getHero
  fetchHero >>= print
  state <- initGQLState
  httpApp <- httpServer state
  Warp.runSettings settings $ WaiWs.websocketsOr defaultConnectionOptions (wsApp state) httpApp
  where
    settings = Warp.setPort 3000 Warp.defaultSettings
    wsApp = gqlSocketApp gqlRoot
    httpServer :: GQLState IO Channel Content -> IO Wai.Application
    httpServer state =
      scottyApp $ do
        post "/" $ raw =<< (liftIO . interpreter gqlRoot state =<< body)
        get "/" $ file "examples/index.html"
        get "/schema.gql" $ raw (toGraphQLDocument gqlRoot)
        post "/mythology" $ raw =<< (liftIO . mythologyApi =<< body)
        get "/mythology" $ file "examples/index.html"
