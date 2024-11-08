module MangaBot.Reddit (
  RedditClientConfig (..),
  Subreddit (..),
  subredditP,
  Comment (..),
  getNewComments,
  getReplies,
  replyToComment,
  AuthInfo (..),
  authInfoP,
  BearerToken (..),
  getToken,
) where

import Relude

import Control.Monad.Logger.Aeson (MonadLogger)
import Data.Aeson.Types (FromJSON, Object, Parser, ToJSON (..), Value (..), object, parseEither, withArray, withObject, (.:), (.=))
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Network.HTTP.Req (GET (..), JsonResponse, MonadHttp, NoReqBody (..), Option, POST (..), ReqBodyUrlEnc (ReqBodyUrlEnc), basicAuth, header, https, responseBody, (/:), (=:))
import Options.Applicative (help, long, metavar, strOption)
import Options.Applicative qualified as Optparse

import MangaBot.Logging (reqL, reqL')

newtype Subreddit = Subreddit Text
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

subredditP :: Optparse.Parser Subreddit
subredditP = Subreddit <$> strOption (long "subreddit" <> metavar "SUBREDDIT" <> help "Subreddit to watch")

data Comment = Comment
  { commentId :: Text
  , body :: Text
  , author :: Text
  , permalink :: Text
  , parentFullname :: Fullname
  , subreddit :: Text
  , articleFullname :: Fullname
  }
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (ToJSON)

parseKind :: Text -> (Object -> Parser a) -> Object -> Parser a
parseKind kindName parseData o = do
  kind <- o .: "kind"
  guard (kind == kindName)
  innerData <- o .: "data"
  parseData innerData

parseListing :: (Object -> Parser a) -> Object -> Parser [a]
parseListing parseChild = parseKind "Listing" $ \listing -> do
  children <- listing .: "children"
  traverse parseChild children

parseComment :: Object -> Parser Comment
parseComment = parseKind "t1" $ \comment -> do
  commentId <- comment .: "id"
  body <- comment .: "body"
  author <- comment .: "author"
  permalink <- comment .: "permalink"
  parentFullname <- comment .: "parent_id"
  subreddit <- comment .: "subreddit"
  articleFullname <- comment .: "link_id"
  pure Comment{commentId, body, author, permalink, parentFullname, subreddit, articleFullname}

newtype Fullname = Fullname Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, FromJSON, ToJSON)

fullnameToId :: Fullname -> Text
fullnameToId (Fullname fullname) = case T.take 3 fullname of
  "t1_" -> uid
  "t2_" -> uid
  "t3_" -> uid
  "t4_" -> uid
  "t5_" -> uid
  "t6_" -> uid
  _ -> error $ "invalid fullname: " <> show fullname
 where
  uid = T.drop 3 fullname

data RedditClientConfig = RedditClientConfig
  { owner :: Text
  , bearerToken :: BearerToken
  }

makeHeaders :: RedditClientConfig -> Option s
makeHeaders RedditClientConfig{bearerToken, owner} =
  header "User-Agent" ("MangaBot v0.1.0 (by /u/" <> encodeUtf8 owner <> ")")
    <> header "Authorization" ("Bearer " <> bearerToken.token)

getNewComments :: (MonadLogger m, MonadReader RedditClientConfig m, MonadHttp m) => Subreddit -> m (Either String [Comment])
getNewComments (Subreddit s) = do
  headers <- makeHeaders <$> ask
  let url = https "oauth.reddit.com" /: "r" /: s /: "comments.json"
      queryParams = "limit" =: (100 :: Int)

  response :: JsonResponse Value <- reqL GET url NoReqBody (headers <> queryParams)
  pure $ flip parseEither (responseBody response) $ withObject "Listing" $ parseListing parseComment

getReplies :: (MonadLogger m, MonadReader RedditClientConfig m, MonadHttp m) => Comment -> m (Either String [Comment])
getReplies Comment{commentId, subreddit, articleFullname} = do
  headers <- makeHeaders <$> ask
  let url =
        https "oauth.reddit.com"
          /: "r"
          /: subreddit
          /: "comments"
          /: fullnameToId articleFullname
          /: "comment"
          /: commentId
      queryParams = "limit" =: (100 :: Int)

  response :: JsonResponse Value <- reqL GET url NoReqBody (headers <> queryParams)
  -- The top-level response is a two-element array, where the second element is a
  -- Listing of Comment objects.
  pure $ flip parseEither (responseBody response) $ withArray "Response" $ \a -> case toList a of
    [_, Object commentResponse] -> parseReplies commentResponse
    _ -> fail "unexpected response shape"
 where
  parseReplies = (<<$>>) concat . parseListing $ parseKind "t1" $ \o -> do
    replies <- o .: "replies"
    case replies of
      Object rs -> parseListing parseComment rs
      String "" -> pure []
      _ -> fail "unexpected replies shape"

replyToComment :: (MonadLogger m, MonadReader RedditClientConfig m, MonadHttp m) => Comment -> Text -> m ()
replyToComment comment reply = do
  headers <- makeHeaders <$> ask
  let url = https "oauth.reddit.com" /: "api" /: "comment"
      formdata = ("parent" =: ("t1_" <> comment.commentId)) <> ("text" =: reply)

  void $ reqL' POST url (ReqBodyUrlEnc formdata) headers

data AuthInfo = AuthInfo
  { clientId :: Text
  , clientSecret :: Text
  , username :: Text
  , password :: Text
  }
  deriving stock (Show)

instance ToJSON AuthInfo where
  toJSON AuthInfo{clientId, username} =
    object
      [ "clientId" .= clientId
      , "clientSecret" .= String "<REDACTED>"
      , "username" .= username
      , "password" .= String "<REDACTED>"
      ]

authInfoP :: Optparse.Parser AuthInfo
authInfoP =
  AuthInfo
    <$> strOption (long "client-id" <> metavar "CLIENT_ID" <> help "OAuth2 client ID")
    <*> strOption (long "client-secret" <> metavar "SECRET" <> help "OAuth2 client secret")
    <*> strOption (long "username" <> metavar "USERNAME" <> help "Reddit bot username")
    <*> strOption (long "password" <> metavar "PASSWORD" <> help "Reddit bot password")

data BearerToken = BearerToken
  { token :: ByteString
  , expiresAt :: UTCTime
  }
  deriving stock (Show)

getToken :: (MonadLogger m, MonadHttp m) => Text -> AuthInfo -> m (Either String BearerToken)
getToken owner AuthInfo{username, password, clientId, clientSecret} = do
  let url = https "www.reddit.com" /: "api" /: "v1" /: "access_token"
      queryParams =
        ("grant_type" =: ("password" :: Text))
          <> ("username" =: username)
          <> ("password" =: password)
      headers =
        header "User-Agent" ("MangaBot v0.1.0 (by /u/" <> encodeUtf8 owner <> ")")
          <> basicAuth (encodeUtf8 clientId) (encodeUtf8 clientSecret)

  response :: JsonResponse Value <- reqL POST url NoReqBody (queryParams <> headers)
  now <- liftIO getCurrentTime
  pure $ flip parseEither (responseBody response) $ withObject "BearerToken" $ \o -> do
    token :: Text <- o .: "access_token"
    expiresIn :: Int <- o .: "expires_in"
    pure BearerToken{token = encodeUtf8 token, expiresAt = addUTCTime (fromIntegral expiresIn) now}
