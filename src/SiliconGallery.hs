{-# LANGUAGE OverloadedStrings #-}

module SiliconGallery (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (forever)
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy
import Data.Foldable
import qualified Data.List
import qualified Data.List as L
import qualified Data.Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Discord
import DiscordModel
import EnvVars (AppConfig (..), readAppConfig)
import Error (Error (ParseHtmlFailed), Outcome, OutcomeIO, errorMessage)
import FileStorage (getPostedEvents, storePostedEvent)
import Model (Event (..), EventImage (EventImage), EventLink (EventLink), EventName (EventName), toID)
import SHScraper (scrapeWeb)

-- | Main application entry point
mainImpl :: OutcomeIO ()
mainImpl = do
  liftIO $ putStrLn "Starting..."

  liftIO $ putStrLn "Loading config..."
  config <- readAppConfig
  let sendDiscordMessage = Discord.sendDiscordMessage config.discordBotToken config.discordChannelID

  -- Closes in case any exception or error occurs
  forever $ do
    liftIO $ putStrLn "Scraping web"
    events <- scrapeWeb
    liftIO . putStrLn $ "Found " ++ show (length events) ++ " events"
    events <- pure $ L.take config.postNLatest events
    events <- pure $ Data.List.reverse events

    postedIds <- getPostedEvents config.fileConfig
    events <- pure $ L.filter (\event -> Data.Set.notMember (toID event) postedIds) events

    liftIO . putStrLn $ "Sending " ++ show (length events) ++ " new events..."
    forM_ events $ \event -> do
      liftIO . putStrLn $ "Sending message for " ++ show event
      sendDiscordMessage $ eventToMessage event
      liftIO . putStrLn $ "Message sent"
      storePostedEvent config.fileConfig $ toID event

    liftIO . putStrLn $ "Done, sleeping for " ++ show config.refreshDelayMinutes ++ " minutes..."
    liftIO $ threadDelay (config.refreshDelayMinutes * 60 * 1000000)

main :: IO ()
main = do
  res <- runExceptT mainImpl
  case res of
    Left err -> print $ errorMessage err
    Right _ -> pure ()
