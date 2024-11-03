{-# OPTIONS_GHC -Wno-orphans #-}

module MangaBot.Orphans () where

import Relude

import Control.Monad.Logger.Aeson (LoggingT)
import Data.Aeson.Types (ToJSON (..), Value, object, (.=))
import Data.CaseInsensitive (original)
import Network.HTTP.Client (Request (..), RequestBody (..), Response (..))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Req (MonadHttp (..))
import Network.HTTP.Types (Header, HeaderName, HttpVersion, statusCode)
import Relude.Extra (member)

instance (MonadHttp m) => MonadHttp (LoggingT m) where
  handleHttpException = lift . handleHttpException

show' :: (Show a) => a -> Value
show' = show @Value

utf8ToJSON :: (ConvertUtf8 Text from) => from -> Value
utf8ToJSON = toJSON . decodeUtf8 @Text

instance ToJSON Request where
  toJSON request =
    object
      [ "method" .= utf8ToJSON (method request)
      , "secure" .= secure request
      , "host" .= utf8ToJSON (host request)
      , "port" .= port request
      , "path" .= utf8ToJSON (path request)
      , "queryString" .= utf8ToJSON (queryString request)
      , "requestHeaders" .= headersToJSON (redactHeaders request) (requestHeaders request)
      , "requestBody" .= requestBody request
      , "proxy" .= proxy request
      , "redirectCount" .= redirectCount request
      , "responseTimeout" .= show' (responseTimeout request)
      , "cookieJar" .= fmap show' (cookieJar request)
      , "requestVersion" .= show' (requestVersion request)
      ]

instance ToJSON RequestBody where
  toJSON (RequestBodyLBS lbs) = utf8ToJSON lbs
  toJSON (RequestBodyBS bs) = utf8ToJSON bs
  toJSON (RequestBodyBuilder _ _) = "<BUILDER>"
  toJSON (RequestBodyStream _ _) = "<STREAM>"
  toJSON (RequestBodyStreamChunked _) = "<STREAMCHUNKED>"
  toJSON (RequestBodyIO _) = "<IO>"

instance ToJSON HTTP.Proxy where
  toJSON HTTP.Proxy{proxyHost, proxyPort} =
    object
      [ "host" .= utf8ToJSON proxyHost
      , "port" .= proxyPort
      ]

instance (ToJSON body) => ToJSON (Response body) where
  toJSON response =
    object
      [ "status" .= statusCode (responseStatus response)
      , "version" .= responseVersion response
      , "headers" .= headersToJSON mempty (responseHeaders response)
      , "body" .= responseBody response
      , "cookieJar" .= show' (responseCookieJar response)
      ]

instance ToJSON HttpVersion where
  toJSON = show

headersToJSON :: Set HeaderName -> [Header] -> Value
headersToJSON toRedact =
  toJSON . map (\(header, value) -> (utf8ToJSON (original header), if header `member` toRedact then "<REDACTED>" else utf8ToJSON value))
