{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Modules store information about posted albums.
-- | A list of event IDs is stored in a file.
-- | This does not scale, but for our use case this is fine
-- | The EventID is stored in the form of the event name along its ID,
-- | e.g. 334-4-blokove-hry-ls25
module FileStorage (FileConfig, createConfig, getPostedEvents, storePostedEvent) where

import qualified Data.IntMap as Set
import qualified Data.List
import Data.Set (Set, empty, fromList)
import qualified Data.String as String
import Error (Outcome)
import Model (EventID (..))
import System.Directory

newtype FileConfig = FileConfig {filepath :: String}

-- | Creates a config from a file path
createConfig :: String -> FileConfig
createConfig = FileConfig

-- | Gets the latest posted event ID if available
getPostedEvents :: FileConfig -> IO (Outcome (Set EventID))
getPostedEvents (FileConfig {filepath = filepath}) = do
  mcontent <- readFileOrNothing filepath
  case mcontent of
    Nothing -> return $ Right empty
    Just content -> do
      let lines = String.lines content
      let valid = Data.List.filter (/= "") lines
      return $ Right $ fromList $ fmap EventID valid

readFileOrNothing :: String -> IO (Maybe String)
readFileOrNothing filepath = do
  exists <- doesFileExist filepath
  if exists
    then Just <$> readFile filepath
    else return Nothing

-- | Stores the latest event ID
storePostedEvent :: FileConfig -> EventID -> IO (Outcome ())
storePostedEvent (FileConfig {filepath = filepath}) event = do
  res <- appendFile filepath $ id ++ "\n"
  return $ Right res
  where
    EventID id = event
