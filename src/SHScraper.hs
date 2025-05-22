{-# LANGUAGE OverloadedStrings #-}

module SHScraper (scrapeWeb, scrapeHtml) where

import Control.Applicative ((<|>))
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.List
import Error (Error (ParseHtmlFailed), Outcome, OutcomeIO)
import Model (Event (Event), EventImage (..), EventLink (..), EventName (..), eventIDFromLink)
import Text.HTML.Scalpel
import Util (toOutcome)

-- | HTTPS is not used as the destination server
-- | does not support Extended Main Secret TLS extension
-- | that Haskell http packages now requires
-- | TODO try to overcome
url :: String
url = "http://www.siliconhill.cz/photogalleries"

scrapeWeb :: OutcomeIO [Event]
scrapeWeb = do
  content <- liftIO $ scrapeURL url content
  rawEvents <- liftEither $ toOutcome (ParseHtmlFailed Nothing) content
  liftEither $ withIDs rawEvents

scrapeHtml :: String -> Outcome [Event]
scrapeHtml html = do
  rawEvents <- toOutcome (ParseHtmlFailed (Just html)) (scrapeStringLike html content)
  withIDs rawEvents

withIDs :: [(EventName, EventLink, EventImage)] -> Outcome [Event]
withIDs rawEvents = do
  ids <- mapM (\(name, link, img) -> eventIDFromLink link) rawEvents
  return $ zipWith (\id (name, link, img) -> Event id name link img) ids rawEvents

content :: Scraper String [(EventName, EventLink, EventImage)]
content = root
  where
    root :: Scraper String [(EventName, EventLink, EventImage)]
    root = chroot ("div" @: ["id" @= "content"] // "div" @: [hasClass "inner-content-full"]) events

    events :: Scraper String [(EventName, EventLink, EventImage)]
    events = chroots ("div" // "div") event

    event :: Scraper String (EventName, EventLink, EventImage)
    event = (,,) <$> eventName <*> eventLink <*> eventImage

    eventName :: Scraper String EventName
    eventName = fmap EventName $ text $ "div" // "div" // "a"

    eventLink :: Scraper String EventLink
    eventLink = fmap EventLink $ attr "href" $ "div" // "div" // "a"

    eventImage :: Scraper String EventImage
    eventImage = fmap EventImage $ attr "src" $ "div" // "img"
