module Util where

import Error (Error, Outcome)

toOutcome :: Error -> Maybe a -> Outcome a
toOutcome err (Just x) = Right x
toOutcome err Nothing = Left err
