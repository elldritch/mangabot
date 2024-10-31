module MangaBotSpec (spec) where

import Relude

import Test.Hspec (Spec, describe, it, shouldBe)

import MangaBot (parseComment)
import MangaBot.Reddit (Comment (..))

spec :: Spec
spec = do
  describe "parseComment" $ do
    it "returns all of the name mentions in a comment" $ do
      parseComment (comment "Test comment mentions [[mention1]] and [[mention2]].")
        `shouldBe` ["mention1", "mention2"]
      parseComment (comment "This is a test comment that mentions one of my faves, \\[\\[There's No Way I Could Date My Fave!\\]\\].")
        `shouldBe` ["There's No Way I Could Date My Fave!"]
 where
  comment body =
    Comment
      { fullname = "t1_someid123"
      , body
      , author = "test-comment-author"
      , permalink = "/r/test_subreddit/comments/anotherid456/this_is_a_test_post/someid123/"
      , parentId = "t3_anotherid456"
      , linkTitle = "This is a test post"
      , linkAuthor = "test-post-author"
      , linkId = "t3_anotherid456"
      , linkPermalink = "https://reddit.com/r/test_subreddit/comments/anotherid456/this_is_a_test_post/"
      }
