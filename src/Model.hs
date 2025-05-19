{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}

module Model (EventName (..), EventImage (..), EventLink (..), Event (..), EventID (..)) where

newtype EventName = EventName String
  deriving (Show, Read, Eq, Ord)

newtype EventLink = EventLink String
  deriving (Show, Read, Eq, Ord)

newtype EventImage = EventImage String
  deriving (Show, Read, Eq, Ord)

newtype EventID = EventID String
  deriving (Show, Read, Eq, Ord)

data Event = Event
  { name :: EventName,
    link :: EventLink,
    image :: EventImage
  }
  deriving (Show, Read, Eq, Ord)

toID :: Event -> EventID
toID event = EventID link
  where
    EventLink link = event.link
