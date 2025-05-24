{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Model
  ( EventName (..),
    EventImage (..),
    EventLink (..),
    Event (..),
    EventID,
    eventIDFromLink,
    eventIDFromFile,
    eventIDForFile,
  )
where

import Consts (urlPhotogalleryPrefix)
import Control.Monad (join, (<=<))
import Data.Text as T
import qualified Data.Text as Data.String
import Error (Error (..), Outcome)
import Util (toOutcome)

newtype EventName = EventName String
  deriving (Show, Read, Eq, Ord)

newtype EventLink = EventLink String
  deriving (Show, Read, Eq, Ord)

newtype EventImage = EventImage String
  deriving (Show, Read, Eq, Ord)

newtype EventID = EventID String
  deriving (Show, Read, Eq, Ord)

-- | A photo album from an event at Strahov
data Event = Event
  { id :: EventID,
    name :: EventName,
    link :: EventLink,
    image :: EventImage
  }
  deriving (Show, Read, Eq, Ord)

-- | Creates an event ID from an event link
-- by keeping only the last segment of the link
eventIDFromLink :: EventLink -> Outcome EventID
eventIDFromLink event = fmap EventID id
  where
    EventLink link = event
    id =
      toOutcome CannotParseEventID {attemptedString = T.pack link}
        . fmap T.unpack
        . (takeNonEmtpy <=< T.stripPrefix (urlPhotogalleryPrefix <> "/"))
        . T.pack
        $ link

takeNonEmtpy :: T.Text -> Maybe T.Text
takeNonEmtpy str = if str == "" then Nothing else Just str

-- | Creates an event ID from string loaded from a storage file
-- No validation is done as there is no strict structure of the ID
eventIDFromFile :: String -> EventID
eventIDFromFile = EventID

-- | Get IDs implementation that can be stored and loaded back using `eventIDFromFile`
eventIDForFile :: EventID -> String
eventIDForFile eventID = id
  where
    EventID id = eventID
