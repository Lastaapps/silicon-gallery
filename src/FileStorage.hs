-- | Modules store information about posted albums.
-- | A list of event IDs is stored in a file.
-- | This does not scale, but for our use case this is fine
-- | The EventID is stored in the form of the event name along its ID,
-- | e.g. 334-4-blokove-hry-ls25
module FileStorage (FileConfig, createConfig, getPostedEventIDs, storePostedEvent) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.IntMap as Set
import qualified Data.List
import Data.Set (Set, empty, fromList)
import qualified Data.String as String
import Error (Outcome, OutcomeIO)
import Model (EventID (..), eventIDForFile, eventIDFromFile)
import System.Directory

newtype FileConfig = FileConfig {filepath :: String}
  deriving (Show, Read, Eq)

-- | Creates a config from a file path
createConfig :: String -> FileConfig
createConfig = FileConfig

-- | Get all the posted event IDs from the file given
getPostedEventIDs :: FileConfig -> OutcomeIO (Set EventID)
getPostedEventIDs (FileConfig {filepath}) = do
  mcontent <- liftIO $ readFileOrNothing filepath
  case mcontent of
    Nothing -> return empty
    Just content -> do
      let lines = String.lines content
      let valid = Data.List.filter (/= "") lines
      return $ fromList $ fmap eventIDFromFile valid

-- | Read content of a file or
-- return empty string if the file does not exit
readFileOrNothing :: String -> IO (Maybe String)
readFileOrNothing filepath = do
  exists <- doesFileExist filepath
  if exists
    then Just <$> readFile filepath
    else return Nothing

-- | Stores a posted event ID to a file
storePostedEvent :: FileConfig -> EventID -> OutcomeIO ()
storePostedEvent (FileConfig {filepath}) eventID = do
  liftIO . appendFile filepath $ eventIDForFile eventID ++ "\n"
