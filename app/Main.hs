module Main (main) where

import Relude
import Relude.Extra.Map (insert, lookup)

import Control.Concurrent (threadDelay)
import Control.Monad.Logger.Aeson (Message (..), MonadLogger, logDebug, logError, logInfo, runStdoutLoggingT, withThreadContext, (.=))
import Data.Aeson.Types (ToJSON)
import Data.Time (addUTCTime, getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Req (defaultHttpConfig, runReq)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, help, helper, info, long, metavar, progDesc, strOption, switch)

import MangaBot (parseMentions, pickManga, renderReply)
import MangaBot.Mangadex (searchManga)
import MangaBot.Orphans ()
import MangaBot.Reddit (AuthInfo (..), BearerToken (..), Comment (..), RedditClientConfig (..), Subreddit, authInfoP, getNewComments, getReplies, getToken, replyToComment, subredditP)

data Options = Options
  { authInfo :: AuthInfo
  , subreddit :: Subreddit
  , owner :: Text
  , dryRun :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

optionsP :: Parser Options
optionsP =
  Options
    <$> authInfoP
    <*> subredditP
    <*> strOption (long "owner" <> metavar "USERNAME" <> help "Username of the bot operator")
    <*> switch (long "dry-run" <> help "Don't actually reply to comments")

argparser :: ParserInfo Options
argparser = info (optionsP <**> helper) (fullDesc <> progDesc "MangaBot")

secondsToMicro :: Int -> Int
secondsToMicro = (* 1000000)

data MangaBotState = MangaBotState
  { bearerTokenCache :: Maybe BearerToken
  , repliesCache :: Map Comment [Comment]
  }

main :: IO ()
main = runReq defaultHttpConfig $ runStdoutLoggingT $ do
  options <- liftIO $ execParser argparser
  hSetBuffering stdout LineBuffering

  logInfo $ "Starting MangaBot" :# ["options" .= options]
  -- Watch the subreddit for new comments.
  foreverWithState MangaBotState{bearerTokenCache = Nothing, repliesCache = mempty} $ do
    runId <- liftIO nextRandom
    withThreadContext ["runId" .= runId] $ do
      logDebug "Starting run"

      result <- runExceptT $ do
        -- Refresh or acquire an access token. Even though this is not
        -- technically needed for some of the public read API requests, Reddit
        -- will block requests that don't have an access token.
        logDebug "Acquiring access token"
        let getAndSaveToken = do
              newToken <- hoistEither =<< getToken options.owner options.authInfo
              modify $ \s -> s{bearerTokenCache = Just newToken}
              pure newToken
        bearerToken <- do
          oldToken <- gets (.bearerTokenCache)
          case oldToken of
            Nothing -> do
              logDebug "No existing token, acquiring new token"
              newToken <- getAndSaveToken
              logDebug "Acquired new token"
              pure newToken
            Just token -> do
              logDebug $ "Found existing token" :# ["expires_at" .= token.expiresAt]
              now <- liftIO getCurrentTime
              if token.expiresAt > addUTCTime 10 now
                then do
                  logDebug "Token is still valid, using it"
                  pure token
                else do
                  logDebug "Token is expired, acquiring new token"
                  newToken <- getAndSaveToken
                  logDebug "Acquired new token"
                  pure newToken
        let runReddit :: ReaderT RedditClientConfig m a -> m a
            runReddit = usingReaderT (RedditClientConfig options.owner bearerToken)
            runRedditE = hoistEither <=< runReddit

        -- TODO: This only retrieves the first page of new comments (at most
        -- 100). It's possible that there could be more than 100 new comments
        -- since the last run 10 seconds ago, but that's pretty unlikely. If
        -- that happens, we'll need to implement something that saves the most
        -- recently replied-to comment and traverses the response of the new
        -- comments API until we find a comment that has been replied to.
        logDebug "Retrieving new comments"
        comments <- runRedditE $ getNewComments options.subreddit
        logDebug $ "Retrieved new comments" :# ["comments" .= map (.permalink) comments]

        -- Loop through every new comment.
        replies <- fmap catMaybes $ forM comments $ \comment ->
          withThreadContext ["comment" .= comment.permalink] $ runMaybeT $ do
            -- Don't reply to comments that don't mention any manga.
            logDebug "Parsing mentions"
            let mentions = parseMentions comment
            logDebug $ "Parsed mentions" :# ["mentions" .= mentions]
            guard' "Comment had no mentions" (not . null $ mentions)

            -- Don't reply to comments that the bot has already replied to.
            logDebug "Retrieving replies"
            repliesCache <- gets (.repliesCache)
            replies <- case comment `lookup` repliesCache of
              Just replies -> do
                logDebug "Found replies in cache"
                pure replies
              Nothing -> do
                -- TODO: If this caching is not sufficient, we can implement
                -- something more sophisticated. For example, we could use a
                -- single API call to retrieve the whole comment forest for an
                -- article, and store all (comment, reply) pairs for that
                -- article in cache, so that we can make only a single API
                -- request per article rather than per comment.
                logDebug "Retrieving replies from Reddit"
                replies <- lift $ runRedditE $ getReplies comment
                logDebug "Retrieved replies from Reddit"
                -- TODO: This will eventually run out of memory. We should
                -- implement some sort of eviction. A big enough LRU should
                -- probably suffice. It just needs to be enough that the replies
                -- of the 100 newest comments are handled.
                modify $ \s -> s{repliesCache = insert comment replies repliesCache}
                pure replies
            logDebug $ "Retrieved replies" :# ["replies" .= map (.permalink) replies]
            guard' "Comment already had reply" (not (any ((options.authInfo.username ==) . (.author)) replies))

            -- For all comments that mention a manga and haven't been replied to,
            -- prepare a reply.
            results <- forM mentions $ \mention -> withThreadContext ["mention" .= mention] $ do
              logDebug "Searching for mentioned manga"
              searchResults <- lift $ runRedditE (searchManga mention)
              logDebug $ "Found search results" :# ["search_results" .= searchResults]
              let result = pickManga mention searchResults
              logDebug $ "Picked manga" :# ["result" .= result]
              pure (mention, result)

            pure (comment, renderReply results)

        -- Refresh or acquire an access token if needed, and perform all queued
        -- replies.
        if null replies
          then logDebug "No replies to perform"
          else forM_ replies $ \(comment, reply) -> withThreadContext ["comment" .= comment] $ do
            logDebug $ "Replying to comment" :# ["reply" .= reply]
            if options.dryRun
              then logDebug "Dry run, skipping reply"
              else runReddit $ replyToComment comment reply
            logDebug "Replied to comment"

      case result of
        Left msg -> logError $ "Encountered errors during run" :# ["message" .= msg]
        Right _ -> pass
      logDebug "Finished run, waiting 10 seconds"
      -- Wait for 10 seconds before checking again.
      liftIO $ threadDelay (secondsToMicro 10)
      logDebug "Finished waiting, starting next run"
 where
  foreverWithState :: (Monad m) => s -> StateT s m a -> m ()
  foreverWithState s a = do
    s' <- execStateT a s
    foreverWithState s' a

  guard' :: (MonadLogger m, Alternative m) => Message -> Bool -> m ()
  guard' _ True = pass
  guard' msg False = logDebug msg >> empty
