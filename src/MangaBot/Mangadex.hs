module MangaBot.Mangadex (searchManga, Manga (..)) where

import Relude

import Data.Aeson (Value, withObject, (.:), (.:?))
import Data.Aeson.Types (parseEither)
import Network.HTTP.Req (GET (..), JsonResponse, MonadHttp, NoReqBody (..), Option, header, https, jsonResponse, req, responseBody, (/:), (=:))

data Manga = Manga
  { mangaId :: Text
  , title :: Text
  , altTitles :: [Text]
  , description :: Maybe Text
  , url :: Text
  , publicationStatus :: Text
  , publicationYear :: Text
  , availableTranslatedLanguages :: [Text]
  , contentRating :: Text
  , tags :: [Text]
  }
  deriving stock (Show)

userAgentHeader :: Option s
userAgentHeader = header "User-Agent" "MangaBot v0.1.0"

searchManga :: (MonadHttp m) => Text -> m (Either String [Manga])
searchManga needle = do
  let url = https "api.mangadex.org" /: "manga"
      options = userAgentHeader <> "title" =: needle

  response :: JsonResponse Value <- req GET url NoReqBody jsonResponse options
  pure $ flip parseEither (responseBody response) $ withObject "collection" $ \o -> do
    result :: Text <- o .: "result"
    guard (result == "ok")
    responseType :: Text <- o .: "response"
    guard (responseType == "collection")
    mangas <- o .: "data"
    forM mangas $ \manga -> do
      mangaType :: Text <- manga .: "type"
      guard (mangaType == "manga")
      mangaId <- manga .: "id"
      attributes <- manga .: "attributes"
      titleTranslations <- attributes .: "title"
      title <- titleTranslations .: "en"
      altTitleTranslations <- attributes .: "altTitles"
      altTitles <- fmap catMaybes $ forM altTitleTranslations $ \altTitle ->
        (altTitle .:? "en") <|> (altTitle .:? "ja-ro")
      descriptionTranslations <- attributes .: "description"
      description <- descriptionTranslations .:? "en"
      publicationStatus <- attributes .: "status"
      publicationYear <- show @Text @Int <$> attributes .: "year"
      availableTranslatedLanguages <- attributes .: "availableTranslatedLanguages"
      contentRating <- attributes .: "contentRating"
      tagsData <- attributes .: "tags"
      tags <- forM tagsData $ withObject "tag" $ \tag -> do
        tagType :: Text <- tag .: "type"
        guard (tagType == "tag")
        tagAttributes <- tag .: "attributes"
        tagNameTranslations <- tagAttributes .: "name"
        tagNameTranslations .: "en"
      pure
        Manga
          { mangaId
          , title
          , altTitles
          , description
          , url = "https://mangadex.org/title/" <> mangaId
          , publicationStatus
          , publicationYear
          , availableTranslatedLanguages
          , contentRating
          , tags
          }
