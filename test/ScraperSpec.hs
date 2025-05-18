{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ScraperSpec where

import Data.List
import qualified Data.Map as Map
import Error (Error (ParseHtmlFailed))
import Model (Event (..), EventImage (EventImage), EventLink (EventLink), EventName (EventName))
import SHScraper (scrapeHtml, scrapeWeb)
import Test.Hspec

spec :: Spec
spec = do
  describe "valid pages" $ do
    let baseRes =
          Right
            [ Event
                { name = EventName "4. Blokové hry LS25",
                  link = EventLink "/photogalleries/334-4-blokove-hry-ls25",
                  image = EventImage "/photogalleries/334/thumb_dscf9336exp.jpg"
                },
              Event
                { name = EventName "Výjezdní zasedání LS 25",
                  link = EventLink "/photogalleries/333-vyjezdni-zasedani-ls-25",
                  image = EventImage "/photogalleries/333/thumb_vyjezd33.jpg"
                },
              Event
                { name = EventName "Velikonoční párty B3",
                  link = EventLink "/photogalleries/332-velikonocni-party-b3",
                  image = EventImage "/photogalleries/332/thumb_thumb_20250416_022812_2.jpg"
                },
              Event
                { name = EventName "2. Blokové hry LS25",
                  link = EventLink "/photogalleries/331-2-blokove-hry-ls25",
                  image = EventImage "/photogalleries/331/thumb_dscf8977.jpg"
                }
            ]
    it "basic" $ do
      scrapeHtml base `shouldBe` baseRes
    it "no styles" $ do
      scrapeHtml noStyles `shouldBe` baseRes
    it "blank" $ do
      scrapeHtml blank
        `shouldBe` Right
          [ Event
              { name = EventName "",
                link = EventLink "",
                image = EventImage ""
              }
          ]
    it "empty" $ do
      scrapeHtml empty
        `shouldBe` Right []
    it "real page" $ do
      page <- readFile "test/webpage_2025-05-18.html"
      case scrapeHtml page of
        Left err -> fail (show err)
        Right res -> do
          res `shouldSatisfy` (\x -> Data.List.length x >= 305)
    xit "live data" $ do
      scraped <- scrapeWeb
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
