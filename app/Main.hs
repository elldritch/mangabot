module Main (main) where

import Relude

import Control.Concurrent (threadDelay)
import Data.Time (addUTCTime, getCurrentTime)
import Network.HTTP.Req (Req, defaultHttpConfig, runReq)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, helper, info, progDesc)

import MangaBot (parseMentions, pickManga, renderReply)
import MangaBot.Mangadex (searchManga)
import MangaBot.Reddit (AuthInfo (..), BearerToken (..), Comment (..), Subreddit, authInfoP, getNewComments, getReplies, getToken, replyToComment, subredditP)

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

      -- Loop through every new comment.
      replies <- fmap catMaybes $ forM comments $ \comment -> runMaybeT $ do
        let mentions = parseMentions comment
        -- Don't reply to comments that don't mention any manga.
        guard (not . null $ mentions)
        -- Don't reply to comments that the bot has already replied to.
        replies <- lift $ runReq' $ getReplies comment
        guard (not (any ((options.authInfo.username ==) . (.author)) replies))
        -- For all comments that mention a manga and haven't been replied to,
        -- prepare a reply.
        results <- forM mentions $ \mention -> do
          searchResults <- lift $ runReq' (searchManga mention)
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
            if token.expiresAt > addUTCTime 10 now
              then pure token
              else do
                newToken <- runReq' (getToken options.authInfo)
                put $ Just newToken
                pure newToken

      forM_ replies $ \(comment, reply) -> do
        runReq defaultHttpConfig $ replyToComment bearerToken comment reply

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
