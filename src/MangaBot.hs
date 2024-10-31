module MangaBot (parseComment, pickManga, renderReply) where

import Relude

import Data.Char qualified as Char
import Data.Text qualified as T
import Text.Megaparsec (Parsec, between, eof, errorBundlePretty, runParser, takeWhileP, try)
import Text.Megaparsec.Char (char, string)

import MangaBot.Mangadex (Manga (..))
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

-- Picks the first candidate that has either a title or alternate title that is
-- a case-insensitive match to the query.
pickManga :: Text -> [Manga] -> Maybe Manga
pickManga query results = flip find results $ \manga ->
  let q = T.toLower query
   in or $ (T.toLower manga.title == q) : map (\altTitle -> T.toLower altTitle == q) manga.altTitles

renderReply :: [(Text, Maybe Manga)] -> Text
renderReply results = T.intercalate "\n\n---\n\n" (map renderResponse results) <> "\n\n---\n\n" <> footer
 where
  renderResponse :: (Text, Maybe Manga) -> Text
  renderResponse (query, Nothing) = "Could not find manga titled `" <> query <> "`"
  renderResponse (_, Just manga) =
    T.intercalate
      "\n\n"
      ( ["_**[" <> manga.title <> "](" <> manga.url <> ")**_"]
          <> ( case manga.description of
                Nothing -> []
                Just desc -> ["**Description:**", prependEveryLine "> " desc]
             )
          <> [ "**Rating:** " <> capitalize manga.contentRating
             , "**Tags:** " <> T.intercalate ", " (map code manga.tags)
             , "**Publication:** " <> manga.publicationYear <> ", " <> manga.publicationStatus
             , "**Translated languages:** " <> T.intercalate ", " (map code manga.availableTranslatedLanguages)
             ]
      )

  capitalize :: Text -> Text
  capitalize text = T.cons (Char.toUpper (T.head text)) (T.tail text)

  prependEveryLine :: Text -> Text -> Text
  prependEveryLine prefix body = T.intercalate "\n" $ map (prefix <>) $ lines body

  code :: Text -> Text
  code = ("`" <>) . (<> "`")

  footer = "Beep boop, I'm a bot. Want to provide feedback or report a bug? Send me a DM!"
