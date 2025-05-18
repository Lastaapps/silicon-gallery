module Model (EventName (..), EventImage (..), EventLink (..), Event (..)) where

newtype EventName = EventName String
  deriving (Show, Read, Eq)

newtype EventLink = EventLink String
  deriving (Show, Read, Eq)

newtype EventImage = EventImage String
  deriving (Show, Read, Eq)

data Event = Event
  { name :: EventName,
    link :: EventLink,
    image :: EventImage
  }
  deriving (Show, Read, Eq)
