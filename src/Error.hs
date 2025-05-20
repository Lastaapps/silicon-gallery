module Error (Error (..), Outcome) where

data Error
  = ParseHtmlFailed {html :: Maybe String}
  | MissingEvironmentVariable {variableName :: String}
  | CannotParseEnvironmentVariable {variableName :: String, value :: String}
  deriving (Show, Eq)

type Outcome = Either Error
