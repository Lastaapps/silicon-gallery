{-# LANGUAGE OverloadedStrings #-}

module SHScraper (scrapeWeb, scrapeHtml) where

import Control.Applicative ((<|>))
import Error (Error (ParseHtmlFailed), Outcome)
import Model (Event (Event), EventImage (..), EventLink (..), EventName (..))
import Text.HTML.Scalpel
import Util (toOutcome)

-- | HTTPS is not used as the destination server
-- | does not support Extended Main Secret TLS extension
-- | that Haskell http packages now requires
-- | TODO try to overcome
url :: String
url = "http://www.siliconhill.cz/photogalleries"

scrapeWeb :: IO (Outcome [Event])
scrapeWeb = do
  content <- scrapeURL url content
  return $ toOutcome (ParseHtmlFailed Nothing) content

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
