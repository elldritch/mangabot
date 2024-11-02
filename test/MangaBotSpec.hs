module MangaBotSpec (spec) where

import Relude

import Test.Hspec (Spec, describe, it, shouldBe)

import MangaBot (parseMentions)
import MangaBot.Reddit (Comment (..))

spec :: Spec
spec = do
  describe "parseMentions" $ do
    it "returns all of the name mentions in a comment" $ do
      parseMentions (comment "Test comment mentions [[mention1]] and [[mention2]].")
        `shouldBe` ["mention1", "mention2"]
      parseMentions (comment "This is a test comment that mentions one of my faves, \\[\\[There's No Way I Could Date My Fave!\\]\\].")
        `shouldBe` ["There's No Way I Could Date My Fave!"]
 where
  comment body =
    Comment
      { commentId = "someid123"
      , body
      , author = "test-comment-author"
      , permalink = "/r/test_subreddit/comments/anotherid456/this_is_a_test_post/someid123/"
      , articleFullname = "t3_anotherid456"
      , subreddit = "test_subreddit"
      , parentFullname = "t3_anotherid456"
      }
