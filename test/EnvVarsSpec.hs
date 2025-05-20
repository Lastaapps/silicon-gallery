{-# LANGUAGE OverloadedStrings #-}

module EnvVarsSpec where

import EnvVars (AppConfig (..), readAppConfig)
import Error (Error (CannotParseEnvironmentVariable, MissingEvironmentVariable, value, variableName))
import FileStorage (createConfig)
import System.Environment (getEnv, lookupEnv, setEnv, unsetEnv)
import System.Random
import Test.Hspec

randomInt :: IO Int
randomInt = randomIO

randomString :: IO String
randomString = fmap show randomInt

keyDiscordBot :: String
keyDiscordBot = "ALBUM_BOT_DISCORD_BOT_TOKEN"

keyDiscordChannel :: String
keyDiscordChannel = "ALBUM_BOT_DISCORD_CHANNEL_ID"

keyStorageFile :: String
keyStorageFile = "ALBUM_BOT_STORAGE_FILE"

keyRefreshDelaySeconds :: String
keyRefreshDelaySeconds = "ALBUM_BOT_REFRESH_DELAY_SECONDS"

setupEnv :: IO (String, String, String, Int)
setupEnv = do
  dcToken <- randomString
  dcChannel <- randomString
  storageFile <- randomString
  refreshDelaySeconds <- randomInt

  setEnv keyDiscordBot (show dcToken)
  setEnv keyDiscordChannel (show dcChannel)
  setEnv keyStorageFile (show storageFile)
  setEnv keyRefreshDelaySeconds (show refreshDelaySeconds)

  return (dcToken, dcChannel, storageFile, refreshDelaySeconds)

spec :: Spec
spec = do
  describe "valid setup" $ do
    it "basic" $ do
      (dcToken, dcChannel, storageFile, refreshDelaySeconds) <- setupEnv
      config <- readAppConfig
      config
        `shouldBe` Right
          ( AppConfig
              { discordBotToken = show dcToken,
                discordChannelID = show dcChannel,
                fileConfig = createConfig (show storageFile),
                refreshDelaySeconds = refreshDelaySeconds
              }
          )
  describe "missing values" $ do
    it "discord bot token empty" $ do
      setupEnv
      setEnv keyDiscordBot ""
      config <- readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyDiscordBot)
    it "discord bot token none" $ do
      setupEnv
      unsetEnv keyDiscordBot
      config <- readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyDiscordBot)
    it "discord channel empty" $ do
      setupEnv
      setEnv keyDiscordChannel ""
      config <- readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyDiscordChannel)
    it "discord channel none" $ do
      setupEnv
      unsetEnv keyDiscordChannel
      config <- readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyDiscordChannel)
    it "storage file empty" $ do
      setupEnv
      setEnv keyStorageFile ""
      config <- readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyStorageFile)
    it "storage file none" $ do
      setupEnv
      unsetEnv keyStorageFile
      config <- readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyStorageFile)
    it "refresh delay empty" $ do
      setupEnv
      setEnv keyRefreshDelaySeconds ""
      config <- readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyRefreshDelaySeconds)
    it "refresh delay none" $ do
      setupEnv
      unsetEnv keyRefreshDelaySeconds
      config <- readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyRefreshDelaySeconds)
    it "refresh delay not int" $ do
      setupEnv
      let wrongValue = "no no no no no!!!"
      setEnv keyRefreshDelaySeconds wrongValue
      config <- readAppConfig
      config
        `shouldBe` Left
          ( CannotParseEnvironmentVariable
              { variableName = keyRefreshDelaySeconds,
                value = wrongValue
              }
          )
