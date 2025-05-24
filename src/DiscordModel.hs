{-# LANGUAGE DeriveGeneric #-}

-- | Discord API data access objects
module DiscordModel
  ( DiscordBotToken (..),
    DiscordChannelID (..),
    botToken,
    channelID,
    DiscordMessage (..),
    Embed (..),
    EmbedAuthor (..),
    EmbedField (..),
    EmbedFooter (..),
    EmbedImage (..),
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Network.HTTP.Req

newtype DiscordBotToken = DiscordBotToken String
  deriving (Show, Read, Eq, Ord)

botToken :: DiscordBotToken -> T.Text
botToken (DiscordBotToken token) = T.pack token

newtype DiscordChannelID = DiscordChannelID String
  deriving (Show, Read, Eq, Ord)

channelID :: DiscordChannelID -> T.Text
channelID (DiscordChannelID id) = T.pack id

-- | Discord message object.
data DiscordMessage = DiscordMessage
  { content :: Maybe Text,
    tts :: Maybe Bool,
    embeds :: Maybe [Embed]
  }
  deriving (Show, Read, Eq, Generic)

instance ToJSON DiscordMessage where
  toJSON DiscordMessage {..} =
    object $
      filter
        (\(_, v) -> v /= Null)
        [ "content" .= content,
          "tts" .= tts,
          "embeds" .= embeds
        ]

instance FromJSON DiscordMessage

-- | An embed object within a Discord message.
data Embed = Embed
  { title :: Maybe Text,
    description :: Maybe Text,
    url :: Maybe Text,
    timestamp :: Maybe Text,
    color :: Maybe Int, -- Decimal representation of hexadecimal color code
    footer :: Maybe EmbedFooter,
    image :: Maybe EmbedImage,
    thumbnail :: Maybe EmbedThumbnail,
    author :: Maybe EmbedAuthor,
    fields :: Maybe [EmbedField]
  }
  deriving (Show, Read, Eq, Generic)

instance ToJSON Embed where
  toJSON Embed {..} =
    object $
      filter
        (\(_, v) -> v /= Null)
        [ "title" .= title,
          "description" .= description,
          "url" .= url,
          "timestamp" .= timestamp,
          "color" .= color,
          "footer" .= footer,
          "image" .= image,
          "thumbnail" .= thumbnail,
          "author" .= author,
          "fields" .= fields
        ]

instance FromJSON Embed

--------------------------------------------------------------------------------
data EmbedFooter = EmbedFooter
  { text :: Text,
    icon_url :: Maybe Text,
    proxy_icon_url :: Maybe Text
  }
  deriving (Show, Read, Eq, Generic)

instance ToJSON EmbedFooter

instance FromJSON EmbedFooter

-- | Represents the image object in an embed.
data EmbedImage = EmbedImage
  { url :: Text,
    proxy_url :: Maybe Text,
    height :: Maybe Int,
    width :: Maybe Int
  }
  deriving (Show, Read, Eq, Generic)

instance ToJSON EmbedImage

instance FromJSON EmbedImage

-- | Represents the thumbnail object in an embed.
data EmbedThumbnail = EmbedThumbnail
  { url :: Text,
    proxy_url :: Maybe Text,
    height :: Maybe Int,
    width :: Maybe Int
  }
  deriving (Show, Read, Eq, Generic)

instance ToJSON EmbedThumbnail

instance FromJSON EmbedThumbnail

-- | Represents the author object in an embed.
data EmbedAuthor = EmbedAuthor
  { name :: Text,
    url :: Maybe Text,
    icon_url :: Maybe Text,
    proxy_icon_url :: Maybe Text
  }
  deriving (Show, Read, Eq, Generic)

instance ToJSON EmbedAuthor

instance FromJSON EmbedAuthor

-- | Represents a field object in an embed.
data EmbedField = EmbedField
  { name :: Text,
    value :: Text,
    inline :: Maybe Bool
  }
  deriving (Show, Read, Eq, Generic)

instance ToJSON EmbedField

instance FromJSON EmbedField
