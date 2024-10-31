module Main (main) where

import Relude

import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime)
import Network.HTTP.Req (Req, defaultHttpConfig, runReq)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, helper, info, progDesc)

import MangaBot (parseComment, pickManga, renderReply)
import MangaBot.Mangadex (searchManga)
import MangaBot.Reddit (AuthInfo, BearerToken (..), Comment (..), Subreddit, authInfoP, getNewComments, getToken, replyToComment, subredditP)

data Options = Options
  { authInfo :: AuthInfo
  , subreddit :: Subreddit
  }
  deriving stock (Show)

optionsP :: Parser Options
optionsP =
  Options
    <$> authInfoP
    <*> subredditP

argv :: ParserInfo Options
argv = info (optionsP <**> helper) (fullDesc <> progDesc "MangaBot")

secondsToMicro :: Int -> Int
secondsToMicro = (* 1000000)

main :: IO ()
main = do
  options <- execParser argv

  -- Watch the subreddit for new comments.
  foreverWithState Nothing $ do
    result <- runExceptT $ do
      comments <- runReq' (getNewComments options.subreddit)

      -- For every comment that matches the mention format, try to find a
      -- matching manga. If a matching manga is found, prepare a reply.
      let matches = filter (not . null . snd) $ map toMatch comments

      replies <- forM matches $ \(comment, mentions) -> do
        results <- forM mentions $ \mention -> do
          searchResults <- runReq' (searchManga mention)
          let result = pickManga mention searchResults
          pure (mention, result)
        pure (comment, renderReply results)

      -- Refresh or acquire an access token if needed, and perform all queued
      -- replies.
      bearerToken <- do
        oldToken <- get
        case oldToken of
          Nothing -> do
            newToken <- runReq' (getToken options.authInfo)
            put $ Just newToken
            pure newToken
          Just token -> do
            now <- liftIO getCurrentTime
            if token.expiresAt > now
              then pure token
              else do
                newToken <- runReq' (getToken options.authInfo)
                put $ Just newToken
                pure newToken

      forM_ replies $ \(comment, reply) -> do
        runReq defaultHttpConfig $ replyToComment bearerToken comment reply
        print (comment, reply)

      -- TODO: Don't reply to the same comment twice.
      undefined

    case result of
      Left msg -> liftIO $ putStrLn $ "Encountered errors: " <> msg
      Right _ -> pass
    -- Wait for 10 seconds before checking again.
    lift $ threadDelay (secondsToMicro 10)
 where
  runReq' :: (MonadIO m) => Req (Either String a) -> ExceptT String m a
  runReq' = hoistEither <=< runReq defaultHttpConfig

  foreverWithState :: (Monad m) => s -> StateT s m a -> m ()
  foreverWithState s a = do
    s' <- execStateT a s
    foreverWithState s' a

  toMatch :: Comment -> (Comment, [Text])
  toMatch comment = (comment, parseComment comment)
