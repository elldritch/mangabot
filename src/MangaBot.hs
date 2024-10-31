module MangaBot (parseComment) where

import Relude

import Text.Megaparsec (Parsec, between, eof, errorBundlePretty, runParser, takeWhileP, try)
import Text.Megaparsec.Char (char, string)

import MangaBot.Reddit (Comment (..))

type Parser = Parsec Void Text

parseComment :: Comment -> [Text]
parseComment Comment{body} = case runParser commentP "" body of
  Left err -> error $ "Failed to parse comment: " <> toText (errorBundlePretty err)
  Right matches -> matches

commentP :: Parser [Text]
commentP = do
  mentions <- many $ try mentionAndPrefix
  _ <- takeWhileP Nothing $ const True
  eof
  pure mentions

mentionAndPrefix :: Parser Text
mentionAndPrefix = do
  _ <- takeWhileP Nothing $ \s -> s /= '[' && s /= '\\'
  -- It turns out that when you type "[[this]]" into a comment in Reddit, that
  -- can actually be saved either as the literal "[[this]]" if you are using the
  -- Markdown comment editor, OR as the literal "\[\[this\]\]" if you are using
  -- the rich text editor. Of course, these two look completely identical in the
  -- UI. Only their API response representations differ.
  (char '[' *> between (string "[") (string "]]") (takeWhileP Nothing $ \s -> s /= ']'))
    <|> (char '\\' *> between (string "[\\[") (string "\\]\\]") (takeWhileP Nothing $ \s -> s /= '\\'))
