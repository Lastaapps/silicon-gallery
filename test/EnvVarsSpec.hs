module EnvVarsSpec where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import DiscordModel
import EnvVars (AppConfig (..), readAppConfig)
import Error (Error (CannotParseEnvironmentVariable, MissingEvironmentVariable, value, variableName))
import FileStorage
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

keyRefreshDelayMinutes :: String
keyRefreshDelayMinutes = "ALBUM_BOT_REFRESH_DELAY_MINUTES"

keyPostNLatest :: String
keyPostNLatest = "ALBUM_BOT_POST_N_LATEST"

setupEnv :: IO (DiscordBotToken, DiscordChannelID, FileConfig, Int, Int)
setupEnv = do
  dcToken <- randomString
  dcChannel <- randomString
  storageFile <- randomString
  refreshDelayMinutes <- randomInt
  postNLatest <- randomInt

  setEnv keyDiscordBot dcToken
  setEnv keyDiscordChannel dcChannel
  setEnv keyStorageFile (show storageFile)
  setEnv keyRefreshDelayMinutes (show refreshDelayMinutes)
  setEnv keyPostNLatest (show postNLatest)

  return
    ( DiscordBotToken dcToken,
      DiscordChannelID dcChannel,
      createConfig (show storageFile),
      refreshDelayMinutes,
      postNLatest
    )

spec :: Spec
spec = do
  describe "valid setup" $ it "basic" $ do
    (dcToken, dcChannel, storageFile, refreshDelayMinutes, postNLatest) <- setupEnv
    config <- runExceptT readAppConfig
    config
      `shouldBe` Right
        ( AppConfig
            { discordBotToken = dcToken,
              discordChannelID = dcChannel,
              fileConfig = storageFile,
              refreshDelayMinutes = refreshDelayMinutes,
              postNLatest = postNLatest
            }
        )
  describe "missing values" $ do
    it "discord bot token empty" $ do
      setupEnv
      setEnv keyDiscordBot ""
      config <- runExceptT readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyDiscordBot)
    it "discord bot token none" $ do
      setupEnv
      unsetEnv keyDiscordBot
      config <- runExceptT readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyDiscordBot)
    it "discord channel empty" $ do
      setupEnv
      setEnv keyDiscordChannel ""
      config <- runExceptT readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyDiscordChannel)
    it "discord channel none" $ do
      setupEnv
      unsetEnv keyDiscordChannel
      config <- runExceptT readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyDiscordChannel)
    it "storage file empty" $ do
      setupEnv
      setEnv keyStorageFile ""
      config <- runExceptT readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyStorageFile)
    it "storage file none" $ do
      setupEnv
      unsetEnv keyStorageFile
      config <- runExceptT readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyStorageFile)
    it "refresh delay empty" $ do
      setupEnv
      setEnv keyRefreshDelayMinutes ""
      config <- runExceptT readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyRefreshDelayMinutes)
    it "refresh delay none" $ do
      setupEnv
      unsetEnv keyRefreshDelayMinutes
      config <- runExceptT readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyRefreshDelayMinutes)
    it "refresh delay not int" $ do
      setupEnv
      let wrongValue = "no no no no no!!!"
      setEnv keyRefreshDelayMinutes wrongValue
      config <- runExceptT readAppConfig
      config
        `shouldBe` Left
          ( CannotParseEnvironmentVariable
              { variableName = keyRefreshDelayMinutes,
                value = wrongValue
              }
          )
    it "post n latest empty" $ do
      setupEnv
      setEnv keyPostNLatest ""
      config <- runExceptT readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyPostNLatest)
    it "post n latest none" $ do
      setupEnv
      unsetEnv keyPostNLatest
      config <- runExceptT readAppConfig
      config `shouldBe` Left (MissingEvironmentVariable keyPostNLatest)
    it "post n latest not int" $ do
      setupEnv
      let wrongValue = "no no no no no!!!"
      setEnv keyPostNLatest wrongValue
      config <- runExceptT readAppConfig
      config
        `shouldBe` Left
          ( CannotParseEnvironmentVariable
              { variableName = keyPostNLatest,
                value = wrongValue
              }
          )
