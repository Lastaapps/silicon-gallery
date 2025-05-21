{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Discord (sendDiscordMessage) where

import Control.Arrow
import Control.Exception (SomeException, try)
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import DiscordModel (DiscordBotToken, DiscordChannelID, DiscordMessage (..))
import qualified DiscordModel
import Error (Error (..), OutcomeIO)
import GHC.Generics (Generic)
import Network.HTTP.Req

-- | Function to make an authorized POST request to a Discord channel.
-- It takes the channel ID, the bot token, and the `DiscordMessage` object.
-- Returns the response body as a lazy `ByteString`, or an error message.
-- TODO retry on rate limit
sendDiscordMessage :: DiscordBotToken -> DiscordChannelID -> DiscordMessage -> OutcomeIO B.ByteString
sendDiscordMessage botToken channelID message = do
  let discordApiBase = "discord.com" :: T.Text
  let endpoint = ["api", "v10", "channels", DiscordModel.channelID channelID, "messages"] :: [T.Text]
      authHeader = "Bot " <> DiscordModel.botToken botToken
      reqBody = ReqBodyJson message

  let result = try @HttpException $ runReq defaultHttpConfig $ do
        response <-
          req
            POST
            (Prelude.foldl (/:) (https discordApiBase) endpoint)
            reqBody
            bsResponse
            (header "Authorization" (encodeUtf8 authHeader))
        return $ responseBody response

  result2 <- liftIO result
  liftEither $ left (\x -> CannotSendDiscordMessage {httpException = x}) result2
