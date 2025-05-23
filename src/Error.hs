module Error (Error (..), Outcome, OutcomeIO, errorMessage) where

import Control.Monad.Except
import qualified Data.Text as T
import Network.HTTP.Req

-- | Error type for the whole application
data Error
  = ParseHtmlFailed {html :: Maybe String}
  | MissingEnvironmentVariable {variableName :: String}
  | CannotParseEnvironmentVariable {variableName :: String, value :: String}
  | CannotSendDiscordMessage {httpException :: HttpException}
  | CannotParseEventID {attemptedString :: T.Text}
  deriving (Show)

-- HttpException does not implement Eq, so we have to do it ourselves
instance Eq Error where
  (==) ParseHtmlFailed {html = html1} ParseHtmlFailed {html = html2} = html1 == html2
  (==) MissingEnvironmentVariable {variableName = variableName1} MissingEnvironmentVariable {variableName = variableName2} = variableName1 == variableName2
  (==) CannotParseEnvironmentVariable {variableName = variableName1, value = value1} CannotParseEnvironmentVariable {variableName = variableName2, value = value2} = variableName1 == variableName2 && value1 == value2
  (==) CannotSendDiscordMessage {httpException = httpException1} CannotSendDiscordMessage {httpException = httpException2} = show httpException1 == show httpException2
  (==) CannotParseEventID {attemptedString = attemptedString1} CannotParseEventID {attemptedString = attemptedString2} = attemptedString1 == attemptedString2
  (==) _ _ = False

type Outcome = Either Error

type OutcomeIO a = ExceptT Error IO a

-- Convert an error to its appropriate error message
errorMessage :: Error -> String
errorMessage ParseHtmlFailed {html = html} = "Failed to parse HTML: " ++ show html
errorMessage MissingEnvironmentVariable {variableName = variableName} = "Missing environment variable: " ++ variableName
errorMessage CannotParseEnvironmentVariable {variableName = variableName, value = value} = "Cannot parse environment variable " ++ variableName ++ ": " ++ value
errorMessage CannotSendDiscordMessage {httpException = httpException} = "Cannot send Discord message: " ++ show httpException
errorMessage CannotParseEventID {attemptedString = attemptedString} = "Cannot parse event ID: " ++ show attemptedString
