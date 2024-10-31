module MangaBot.Reddit (
  Subreddit (..),
  subredditP,
  Comment (..),
  getNewComments,
  replyToComment,
  AuthInfo (..),
  authInfoP,
  BearerToken (..),
  getToken,
) where

import Relude

import Data.Aeson (Value, withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Network.HTTP.Req (GET (..), JsonResponse, MonadHttp, NoReqBody (..), Option, POST (..), ReqBodyUrlEnc (ReqBodyUrlEnc), basicAuth, header, https, ignoreResponse, jsonResponse, req, responseBody, (/:), (=:))
import Options.Applicative (Parser, help, long, metavar, strOption)

newtype Subreddit = Subreddit Text
  deriving stock (Show)

subredditP :: Parser Subreddit
subredditP = Subreddit <$> strOption (long "subreddit" <> metavar "SUBREDDIT" <> help "Subreddit to watch")

data Comment = Comment
  { fullname :: Text
  , body :: Text
  , author :: Text
  , permalink :: Text
  , parentId :: Text
  , linkTitle :: Text
  , linkAuthor :: Text
  , linkId :: Text
  , linkPermalink :: Text
  }
  deriving stock (Show)

getNewComments :: (MonadHttp m) => Subreddit -> m (Either String [Comment])
getNewComments (Subreddit subreddit) = do
  let url = https "api.reddit.com" /: "r" /: subreddit /: "comments.json"
      headers = userAgentHeader
      queryParams = "limit" =: (100 :: Int)

  response :: JsonResponse Value <- req GET url NoReqBody jsonResponse (headers <> queryParams)
  pure $ flip parseEither (responseBody response) $ withObject "Listing" $ \o -> do
    listingKind :: Text <- o .: "kind"
    guard (listingKind == "Listing")
    listingData <- o .: "data"
    comments <- listingData .: "children"
    forM comments $ \comment -> do
      commentKind :: Text <- comment .: "kind"
      guard (commentKind == "t1")
      commentData <- comment .: "data"
      fullname <- commentData .: "name"
      body <- commentData .: "body"
      author <- commentData .: "author"
      permalink <- commentData .: "permalink"
      parentId <- commentData .: "parent_id"
      linkTitle <- commentData .: "link_title"
      linkAuthor <- commentData .: "link_author"
      linkId <- commentData .: "link_id"
      linkPermalink <- commentData .: "link_permalink"
      pure Comment{body, fullname, author, permalink, parentId, linkTitle, linkAuthor, linkId, linkPermalink}

replyToComment :: (MonadHttp m) => BearerToken -> Comment -> Text -> m ()
replyToComment bearerToken comment reply = do
  let url = https "oauth.reddit.com" /: "api" /: "comment"
      formdata = ("parent" =: comment.fullname) <> ("text" =: reply)
      headers = userAgentHeader <> header "Authorization" ("Bearer " <> bearerToken.token)

  void $ req POST url (ReqBodyUrlEnc formdata) ignoreResponse headers

data AuthInfo = AuthInfo
  { clientId :: Text
  , clientSecret :: Text
  , username :: Text
  , password :: Text
  }
  deriving stock (Show)

authInfoP :: Parser AuthInfo
authInfoP =
  AuthInfo
    <$> strOption (long "client-id" <> metavar "CLIENT_ID" <> help "OAuth2 client ID")
    <*> strOption (long "client-secret" <> metavar "CLIENT_SECRET" <> help "OAuth2 client secret")
    <*> strOption (long "username" <> metavar "USERNAME" <> help "Reddit bot username")
    <*> strOption (long "password" <> metavar "PASSWORD" <> help "Reddit bot password")

data BearerToken = BearerToken
  { token :: ByteString
  , expiresAt :: UTCTime
  }
  deriving stock (Show)

userAgentHeader :: Option s
userAgentHeader = header "User-Agent" "MangaBot v0.1.0"

getToken :: (MonadHttp m) => AuthInfo -> m (Either String BearerToken)
getToken AuthInfo{username, password, clientId, clientSecret} = do
  let url = https "www.reddit.com" /: "api" /: "v1" /: "access_token"
      queryParams =
        ("grant_type" =: ("password" :: Text))
          <> ("username" =: username)
          <> ("password" =: password)
      headers =
        userAgentHeader
          <> basicAuth (encodeUtf8 clientId) (encodeUtf8 clientSecret)

  response :: JsonResponse Value <- req POST url NoReqBody jsonResponse (queryParams <> headers)
  now <- liftIO getCurrentTime
  pure $ flip parseEither (responseBody response) $ withObject "BearerToken" $ \o -> do
    token :: Text <- o .: "access_token"
    expiresIn :: Int <- o .: "expires_in"
    pure BearerToken{token = encodeUtf8 token, expiresAt = addUTCTime (fromIntegral expiresIn) now}
