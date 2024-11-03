module MangaBot.Logging (reqL, reqL') where

import Relude

import Control.Monad.Logger.Aeson (Message (..), MonadLogger, logDebug, (.=))
import Data.Aeson.Types (FromJSON, ToJSON (..))
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Req (HttpBody, HttpBodyAllowed, HttpMethod (..), HttpResponse (..), IgnoreResponse, JsonResponse, MonadHttp (..), Option, ProvidesBody, Url, ignoreResponse, jsonResponse, reqCb)

import MangaBot.Orphans ()

reqL ::
  ( MonadLogger m
  , MonadHttp m
  , HttpMethod method
  , HttpBody body
  , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
  , FromJSON a
  , ToJSON a
  ) =>
  method ->
  Url scheme ->
  body ->
  Option scheme ->
  m (JsonResponse a)
reqL = reqLogged jsonResponse

reqL' ::
  ( MonadLogger m
  , MonadHttp m
  , HttpMethod method
  , HttpBody body
  , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
  ) =>
  method ->
  Url scheme ->
  body ->
  Option scheme ->
  m IgnoreResponse
reqL' = reqLogged ignoreResponse

reqLogged ::
  ( MonadLogger m
  , MonadHttp m
  , HttpMethod method
  , HttpBody body
  , HttpResponse response
  , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
  , ToJSON (HttpResponseBody response)
  ) =>
  Proxy response ->
  method ->
  Url scheme ->
  body ->
  Option scheme ->
  m response
reqLogged resProxy method url body opts = do
  requestID <- liftIO nextRandom
  response <- reqCb method url body resProxy opts $ \req ->
    req <$ logDebug ("Sending request" :# ["request" .= toJSON req, "request_id" .= requestID])
  logDebug $ "Received response" :# ["response" .= toJSON (toVanillaResponse response), "request_id" .= requestID]
  pure response
