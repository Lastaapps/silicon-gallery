{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module SHScraperSpec where

import Control.Monad.Except (runExceptT)
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import Error (Error (CannotParseEventID, ParseHtmlFailed, attemptedString))
import Model (Event (..), EventID, EventImage (EventImage), EventLink (EventLink), EventName (EventName), eventIDFromFile, eventIDFromLink)
import SHScraper (scrapeHtml, scrapeWeb)
import Test.Hspec
import Test.QuickCheck (Testable (property))

eventID :: String -> EventID
eventID = eventIDFromFile

spec :: Spec
spec = do
  describe "valid pages" $ do
    let baseRes =
          Right
            [ Event
                { id = eventID "334-4-blokove-hry-ls25",
                  name = EventName "4. Blokové hry LS25",
                  link = EventLink "/photogalleries/334-4-blokove-hry-ls25",
                  image = EventImage "/photogalleries/334/thumb_dscf9336exp.jpg"
                },
              Event
                { id = eventID "333-vyjezdni-zasedani-ls-25",
                  name = EventName "Výjezdní zasedání LS 25",
                  link = EventLink "/photogalleries/333-vyjezdni-zasedani-ls-25",
                  image = EventImage "/photogalleries/333/thumb_vyjezd33.jpg"
                },
              Event
                { id = eventID "332-velikonocni-party-b3",
                  name = EventName "Velikonoční párty B3",
                  link = EventLink "/photogalleries/332-velikonocni-party-b3",
                  image = EventImage "/photogalleries/332/thumb_thumb_20250416_022812_2.jpg"
                },
              Event
                { id = eventID "331-2-blokove-hry-ls25",
                  name = EventName "2. Blokové hry LS25",
                  link = EventLink "/photogalleries/331-2-blokove-hry-ls25",
                  image = EventImage "/photogalleries/331/thumb_dscf8977.jpg"
                }
            ]
    it "basic" $ do
      scrapeHtml base `shouldBe` baseRes
    it "no styles" $ do
      scrapeHtml noStyles `shouldBe` baseRes
    it "empty" $ do
      scrapeHtml empty
        `shouldBe` Right []
    it "real page" $ do
      page <- readFile "test/webpage_2025-05-18.html"
      case scrapeHtml page of
        Left err -> fail (show err)
        Right res -> do
          res `shouldSatisfy` (\x -> Data.List.length x >= 305)
    -- Use `xit` to skip the test
    it "live data" $ do
      scraped <- runExceptT scrapeWeb
      case scraped of
        Left err -> fail (show err)
        Right res -> do
          res `shouldSatisfy` (\x -> Data.List.length x >= 305)

  describe "invalid pages" $ do
    it "empty" $ do
      scrapeHtml "" `shouldBe` Left (ParseHtmlFailed (Just ""))
    it "no body" $ do
      scrapeHtml "<html></html>" `shouldBe` Left (ParseHtmlFailed (Just "<html></html>"))
    it "no content" $ do
      scrapeHtml "<html><body></body></html>" `shouldBe` Left (ParseHtmlFailed (Just "<html><body></body></html>"))
    it "no root" $ do
      let text = "<html><body><div></div></body></html>"
      scrapeHtml text `shouldBe` Left (ParseHtmlFailed (Just text))
    it "random" $ do
      let text = "lorem ipsum dolor sit amet consectetur adipiscing elit"
      scrapeHtml text `shouldBe` Left (ParseHtmlFailed (Just text))
    it "blank" $ do
      scrapeHtml blank
        `shouldBe` Left CannotParseEventID {attemptedString = ""}
  describe "event ids" $ do
    it "valid link" $ do
      let base = "334-4-blokove-hry-ls25"
      let link = "/photogalleries/" ++ base
      eventIDFromLink (EventLink link) `shouldBe` Right (eventID base)
    it "empty" $ do
      let base = ""
      let link = "/photogalleries/" ++ base
      eventIDFromLink (EventLink link) `shouldBe` Left CannotParseEventID {attemptedString = T.pack link}
    it "random event ids that need to fail" $ property $ \content -> do
      eventIDFromLink (EventLink content)
        `shouldBe` Left CannotParseEventID {attemptedString = T.pack content}

base :: String
base =
  "<body>\
  \  <div id='content'>\
  \      <div class='inner-content-full'>\
  \        <h1><span>Fotogalerie</span></h1>\
  \        \
  \        <div style='float:left; width:270px; padding:10px; margin:0 15px 15px 0; text-align:center;'>\
  \          <a href='/photogalleries/334-4-blokove-hry-ls25'><img alt='Thumb_dscf9336exp' src='/photogalleries/334/thumb_dscf9336exp.jpg' style='padding:5px; background-color:white; border:1px solid silver;' /></a>\
  \        <div><a href='/photogalleries/334-4-blokove-hry-ls25'>4. Blokové hry LS25</a></div>\
  \        </div>\
  \        <div style='float:left; width:270px; padding:10px; margin:0 15px 15px 0; text-align:center;'>\
  \            <a href='/photogalleries/333-vyjezdni-zasedani-ls-25'><img alt='Thumb_vyjezd33' src='/photogalleries/333/thumb_vyjezd33.jpg' style='padding:5px; background-color:white; border:1px solid silver;' /></a>\
  \          <div><a href='/photogalleries/333-vyjezdni-zasedani-ls-25'>Výjezdní zasedání LS 25</a></div>\
  \        </div>\
  \        <div style='float:left; width:270px; padding:10px; margin:0 15px 15px 0; text-align:center;'>\
  \            <a href='/photogalleries/332-velikonocni-party-b3'><img alt='Thumb_thumb_20250416_022812_2' src='/photogalleries/332/thumb_thumb_20250416_022812_2.jpg' style='padding:5px; background-color:white; border:1px solid silver;' /></a>\
  \          <div><a href='/photogalleries/332-velikonocni-party-b3'>Velikonoční párty B3</a></div>\
  \        </div>\
  \          <div class='clear'></div>\
  \        <div style='float:left; width:270px; padding:10px; margin:0 15px 15px 0; text-align:center;'>\
  \            <a href='/photogalleries/331-2-blokove-hry-ls25'><img alt='Thumb_dscf8977' src='/photogalleries/331/thumb_dscf8977.jpg' style='padding:5px; background-color:white; border:1px solid silver;' /></a>\
  \        <div><a href='/photogalleries/331-2-blokove-hry-ls25'>2. Blokové hry LS25</a></div>\
  \      </div>\
  \    </div>\
  \  </div>\
  \</body>"

noStyles :: String
noStyles =
  "<body>\
  \  <div id='content'>\
  \      <div class='inner-content-full'>\
  \        <h1><span>Fotogalerie</span></h1>\
  \        \
  \        <div>\
  \          <a href='/photogalleries/334-4-blokove-hry-ls25'><img alt='Thumb_dscf9336exp' src='/photogalleries/334/thumb_dscf9336exp.jpg' /></a>\
  \        <div><a href='/photogalleries/334-4-blokove-hry-ls25'>4. Blokové hry LS25</a></div>\
  \        </div>\
  \        <div>\
  \            <a href='/photogalleries/333-vyjezdni-zasedani-ls-25'><img alt='Thumb_vyjezd33' src='/photogalleries/333/thumb_vyjezd33.jpg' /></a>\
  \          <div><a href='/photogalleries/333-vyjezdni-zasedani-ls-25'>Výjezdní zasedání LS 25</a></div>\
  \        </div>\
  \        <div>\
  \            <a href='/photogalleries/332-velikonocni-party-b3'><img alt='Thumb_thumb_20250416_022812_2' src='/photogalleries/332/thumb_thumb_20250416_022812_2.jpg' /></a>\
  \          <div><a href='/photogalleries/332-velikonocni-party-b3'>Velikonoční párty B3</a></div>\
  \        </div>\
  \          <div class='clear'></div>\
  \        <div>\
  \            <a href='/photogalleries/331-2-blokove-hry-ls25'><img alt='Thumb_dscf8977' src='/photogalleries/331/thumb_dscf8977.jpg' /></a>\
  \        <div><a href='/photogalleries/331-2-blokove-hry-ls25'>2. Blokové hry LS25</a></div>\
  \      </div>\
  \    </div>\
  \  </div>\
  \</body>"

empty :: String
empty =
  "<body>\
  \  <div id='content'>\
  \      <div class='inner-content-full'>\
  \        <h1><span>Fotogalerie</span></h1>\
  \      </div>\
  \    </div>\
  \  </div>\
  \</body>"

blank :: String
blank =
  "<body>\
  \  <div id='content'>\
  \      <div class='inner-content-full'>\
  \        <h1><span>Fotogalerie</span></h1>\
  \        <div>\
  \          <a href='/photogalleries/334-4-blokove-hry-ls25'><img alt='Thumb_dscf9336exp' src='' /></a>\
  \        <div><a href=''></a></div>\
  \        </div>\
  \      </div>\
  \    </div>\
  \  </div>\
  \</body>"
