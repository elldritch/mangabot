module Main (main) where

import Relude

import Control.Concurrent (threadDelay)
import Network.HTTP.Req (defaultHttpConfig, runReq)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, helper, info, progDesc)

import MangaBot (parseComment)
import MangaBot.Mangadex (searchManga)
import MangaBot.Reddit (AuthInfo, Comment (..), Subreddit, authInfoP, getNewComments, getToken, subredditP)

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
      comments <- runReq defaultHttpConfig $ getNewComments options.subreddit
      comments' <- hoistEither comments

      forM_ comments' $ \comment -> putTextLn comment.body

      -- For every comment that matches the mention format, try to find a matching manga.
      let matches = filter (not . null . snd) $ map toMatch comments'
      print matches
      -- mangas <- runReq defaultHttpConfig $ searchManga "Kanojyo to Himitsu to Koimoyou"
      -- print mangas

      -- If a matching manga is found, prepare a reply.

      -- Refresh or acquire an access token if needed, and perform all queued
      -- replies.
      -- token <- runReq defaultHttpConfig $ getToken options.authInfo
      -- print token

      -- Wait for 10 seconds before checking again.
    case result of
      Left msg -> liftIO $ putStrLn $ "Encountered errors: " <> msg
      Right _ -> pass
    lift $ threadDelay (secondsToMicro 10)
 where
  foreverWithState :: (Monad m) => s -> StateT s m a -> m ()
  foreverWithState s a = do
    s' <- execStateT a s
    foreverWithState s' a

  toMatch :: Comment -> (Comment, [Text])
  toMatch comment = (comment, parseComment comment)
