module Util where

import qualified Data.Text as T
import Error (Error, Outcome)
import Network.HTTP.Req (Url, https, (/:))

-- | Converts `Maybe` to `Outcome` with the given error
toOutcome :: Error -> Maybe a -> Outcome a
toOutcome err (Just x) = Right x
toOutcome err Nothing = Left err

-- | Constructs a `Url` from a `protocol`, `domain`, and path segments
domainWithPath :: (T.Text -> Url schema) -> T.Text -> [T.Text] -> Url schema
domainWithPath protocol domain = Prelude.foldl (/:) (protocol domain)
