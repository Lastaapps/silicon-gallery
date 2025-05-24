{-# LANGUAGE TypeApplications #-}

module Discord (sendDiscordMessage, eventToMessage) where

import qualified Consts (baseUrl, postsAuthor, postsFootnote, siliconHillRed)
import Control.Arrow
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.ByteString as B
import Data.Either (isLeft)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import DiscordModel
import qualified DiscordModel
import Error (Error (..), OutcomeIO)
import GHC.Generics (Generic)
import Model
import qualified Model
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (StatusCodeException), Response (responseHeaders))
import Network.HTTP.Req
import Util (domainWithPath)

-- | Function to make an authorized POST request to a Discord channel.
-- It takes the channel ID, the bot token, and the `DiscordMessage` object.
-- Returns the response body as a lazy `ByteString`, or an error message.
sendDiscordMessage :: DiscordBotToken -> DiscordChannelID -> DiscordMessage -> OutcomeIO B.ByteString
sendDiscordMessage botToken channelID message = do
  let discordApiBase = "discord.com" :: T.Text
  let endpoint = ["api", "v10", "channels", DiscordModel.channelID channelID, "messages"] :: [T.Text]
      authHeader = "Bot " <> DiscordModel.botToken botToken
      reqBody = ReqBodyJson message

  let result = try @Network.HTTP.Req.HttpException $ runReqRateLimiting defaultHttpConfig $ do
        response <-
          req
            POST
            (domainWithPath https discordApiBase endpoint)
            reqBody
            bsResponse
            (header "Authorization" (encodeUtf8 authHeader))
        return $ responseBody response

  result2 <- liftIO result
  liftEither $ left (\x -> CannotSendDiscordMessage {httpException = x}) result2

-- | Handle bad responses from Discord due rate limits -
-- otherwise, we post as fast possible
runReqRateLimiting :: HttpConfig -> Req a -> IO a
runReqRateLimiting config req = do
  result <- try (runReq config req)
  case result of
    Left (VanillaHttpException (HttpExceptionRequest _ (StatusCodeException resp _))) ->
      handleRateLimitException config req resp
    Left _ ->
      -- Re-throw other `HttpExceptions`
      either throwIO pure result
    Right val ->
      pure val

-- | Checks if the caught exception is a rate limit exception and retries
-- if needed
handleRateLimitException :: HttpConfig -> Req a -> Response () -> IO a
handleRateLimitException config req resp = do
  case lookup "X-RateLimit-Reset-After" (responseHeaders resp) of
    Just rawRateLimit -> do
      let rateLimitText = T.decodeUtf8 rawRateLimit
      case reads (T.unpack rateLimitText) :: [(Float, String)] of
        [(ratelimitReset, "")] -> do
          let delayUs = round (ratelimitReset * 1000000)
          liftIO . putStrLn $ "Rate limited. Retrying after " ++ show ratelimitReset ++ " seconds."
          liftIO $ threadDelay delayUs
          runReqRateLimiting config req
        _ ->
          -- If header is present but not a valid float, re-throw original exception
          throwIO $ VanillaHttpException (HttpExceptionRequest (error "Original request info lost") (StatusCodeException resp B.empty))
    Nothing ->
      -- If `X-RateLimit-Reset-After` header is missing, re-throw original exception
      throwIO $ VanillaHttpException (HttpExceptionRequest (error "Original request info lost") (StatusCodeException resp B.empty))

-- | Converts Event to `DiscordMessage`
eventToMessage :: Model.Event -> DiscordMessage
eventToMessage event =
  DiscordMessage
    { content = Nothing,
      tts = Just False,
      embeds =
        Just
          [ Embed
              { title = Just $ T.pack name,
                description = Nothing,
                url = Just $ Consts.baseUrl <> T.pack link,
                timestamp = Nothing,
                color = Just Consts.siliconHillRed,
                footer = Just (EmbedFooter Consts.postsFootnote Nothing Nothing),
                image = Just (EmbedImage (Consts.baseUrl <> T.pack image) Nothing Nothing Nothing),
                thumbnail = Nothing,
                author = Just (EmbedAuthor Consts.postsAuthor (Just Consts.baseUrl) Nothing Nothing),
                fields = Nothing
              }
          ]
    }
  where
    EventName name = event.name
    EventLink link = event.link
    EventImage image = event.image
