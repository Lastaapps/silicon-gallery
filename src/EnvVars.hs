module EnvVars (AppConfig (..), readAppConfig) where

import Error
import FileStorage (FileConfig, createConfig)
import System.Environment (lookupEnv)
import Text.Read
import Util (toOutcome)

data AppConfig = AppConfig
  { discordBotToken :: String,
    discordChannelID :: String,
    fileConfig :: FileConfig,
    refreshDelaySeconds :: Int
  }
  deriving (Show, Read, Eq)

prefix :: String
prefix = "ALBUM_BOT_"

discordBotTokenKey :: String
discordBotTokenKey = prefix ++ "DISCORD_BOT_TOKEN"

discordChannelIDKey :: String
discordChannelIDKey = prefix ++ "DISCORD_CHANNEL_ID"

storageFileKey :: String
storageFileKey = prefix ++ "STORAGE_FILE"

refreshDelaySecondsKey :: String
refreshDelaySecondsKey = prefix ++ "REFRESH_DELAY_SECONDS"

readAppConfig :: IO (Outcome AppConfig)
readAppConfig = do
  discordBotToken <- lookupEnv discordBotTokenKey
  discordBotToken <- pure $ toOutcome (MissingEvironmentVariable discordBotTokenKey) discordBotToken
  discordChannelID <- lookupEnv discordChannelIDKey
  discordChannelID <- pure $ toOutcome (MissingEvironmentVariable discordChannelIDKey) discordChannelID
  storageFile <- lookupEnv storageFileKey
  storageFile <- pure $ toOutcome (MissingEvironmentVariable storageFileKey) storageFile
  let fileConfig = fmap createConfig storageFile
  refreshDelaySeconds <- lookupEnv refreshDelaySecondsKey
  refreshDelaySeconds <- pure $ toOutcome (MissingEvironmentVariable refreshDelaySecondsKey) refreshDelaySeconds
  refreshDelaySeconds <- pure $ (\x -> toOutcome (CannotParseEnvironmentVariable refreshDelaySecondsKey x) (readIntMaybe x)) =<< refreshDelaySeconds

  return $ AppConfig <$> discordBotToken <*> discordChannelID <*> fileConfig <*> refreshDelaySeconds

readIntMaybe :: String -> Maybe Int
readIntMaybe = readMaybe
