{-# LANGUAGE OverloadedStrings #-}

module SHScraper (scrapeWeb, scrapeHtml) where

import Control.Applicative ((<|>))
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.List
import Error (Error (ParseHtmlFailed), Outcome, OutcomeIO)
import Model (Event (Event), EventImage (..), EventLink (..), EventName (..))
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
  liftEither $ toOutcome (ParseHtmlFailed Nothing) content

scrapeHtml :: String -> Outcome [Event]
scrapeHtml html =
  toOutcome (ParseHtmlFailed (Just html)) (scrapeStringLike html content)

content :: Scraper String [Event]
content = root
  where
    root :: Scraper String [Event]
    root = chroot ("div" @: ["id" @= "content"] // "div" @: [hasClass "inner-content-full"]) events

    events :: Scraper String [Event]
    events = chroots ("div" // "div") event

    event :: Scraper String Event
    event = Event <$> eventName <*> eventLink <*> eventImage

    eventName :: Scraper String EventName
    eventName = fmap EventName $ text $ "div" // "div" // "a"

    eventLink :: Scraper String EventLink
    eventLink = fmap EventLink $ attr "href" $ "div" // "div" // "a"

    eventImage :: Scraper String EventImage
    eventImage = fmap EventImage $ attr "src" $ "div" // "img"
