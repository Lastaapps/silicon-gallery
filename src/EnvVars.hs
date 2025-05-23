-- | Module responsible from loading application configuration
-- from environment variables
module EnvVars (AppConfig (..), readAppConfig) where

import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text as T
import DiscordModel
import Error
import FileStorage (FileConfig, createConfig)
import System.Environment (lookupEnv)
import Text.Read
import Util (toOutcome)

-- | Store all the application configuration
data AppConfig = AppConfig
  { discordBotToken :: DiscordBotToken,
    discordChannelID :: DiscordChannelID,
    fileConfig :: FileConfig,
    refreshDelayMinutes :: Int,
    postNLatest :: Int
  }
  deriving (Show, Read, Eq)

-- | Used to prefix all the environment variables
-- to separate then from the environment
prefix :: String
prefix = "ALBUM_BOT_"

discordBotTokenKey :: String
discordBotTokenKey = prefix ++ "DISCORD_BOT_TOKEN"

discordChannelIDKey :: String
discordChannelIDKey = prefix ++ "DISCORD_CHANNEL_ID"

storageFileKey :: String
storageFileKey = prefix ++ "STORAGE_FILE"

refreshDelayMinutesKey :: String
refreshDelayMinutesKey = prefix ++ "REFRESH_DELAY_MINUTES"

postNLatestKey :: String
postNLatestKey = prefix ++ "POST_N_LATEST"

-- | Reads all the required environment variables
-- If any is missing of is in an invalid format,
-- an error is returned.
readAppConfig :: OutcomeIO AppConfig
readAppConfig = do
  discordBotToken <- liftIO $ lookupEnv discordBotTokenKey
  discordBotToken <- liftEither $ toOutcome (MissingEnvironmentVariable discordBotTokenKey) discordBotToken

  discordChannelID <- liftIO $ lookupEnv discordChannelIDKey
  discordChannelID <- liftEither $ toOutcome (MissingEnvironmentVariable discordChannelIDKey) discordChannelID

  storageFile <- liftIO $ lookupEnv storageFileKey
  storageFile <- liftEither $ toOutcome (MissingEnvironmentVariable storageFileKey) storageFile

  let fileConfig = createConfig storageFile
  refreshDelayMinutes <- liftIO $ lookupEnv refreshDelayMinutesKey
  refreshDelayMinutes <- liftEither $ toOutcome (MissingEnvironmentVariable refreshDelayMinutesKey) refreshDelayMinutes
  refreshDelayMinutes <- liftEither $ (\x -> toOutcome (CannotParseEnvironmentVariable refreshDelayMinutesKey x) (readIntMaybe x)) refreshDelayMinutes

  postNLatest <- liftIO $ lookupEnv postNLatestKey
  postNLatest <- liftEither $ toOutcome (MissingEnvironmentVariable postNLatestKey) postNLatest
  postNLatest <- liftEither $ (\x -> toOutcome (CannotParseEnvironmentVariable postNLatestKey x) (readIntMaybe x)) postNLatest

  return $
    AppConfig
      { discordBotToken = DiscordBotToken discordBotToken,
        discordChannelID = DiscordChannelID discordChannelID,
        fileConfig = fileConfig,
        refreshDelayMinutes = refreshDelayMinutes,
        postNLatest = postNLatest
      }

readIntMaybe :: String -> Maybe Int
readIntMaybe = readMaybe
