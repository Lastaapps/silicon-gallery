module Util where

import qualified Data.Text as T
import Error (Error, Outcome)
import Network.HTTP.Req (Url, https, (/:))

toOutcome :: Error -> Maybe a -> Outcome a
toOutcome err (Just x) = Right x
toOutcome err Nothing = Left err

domainWithPath :: (T.Text -> Url schema) -> T.Text -> [T.Text] -> Url schema
domainWithPath protocol domain = Prelude.foldl (/:) (protocol domain)
