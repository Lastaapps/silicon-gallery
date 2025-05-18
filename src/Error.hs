{-# LANGUAGE GADTs #-}

module Error (Error (..), Outcome) where

data Error
  = ParseHtmlFailed {html :: Maybe String}
  deriving (Show, Eq)

type Outcome = Either Error
