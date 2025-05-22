module FileStorageSpec where

import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
import qualified Data.List
import Data.Set (empty, fromList)
import qualified Data.String
import Debug.Trace (trace)
import FileStorage (createConfig, getPostedEventIDs, storePostedEvent)
import Model (EventID (..), eventIDFromFile)
import System.Directory (removeFile)
import System.IO
import Test.Hspec
import Test.QuickCheck

filename :: String
filename = "test/store.txt"

eventID :: String -> EventID
eventID = eventIDFromFile

spec :: Spec
spec = afterAll_ (removeFile filename) $ do
  describe "valid pages" $ do
    it "read empty" $ do
      res <- runExceptT $ getPostedEventIDs (createConfig "test/posted_emtpy.txt")
      res `shouldBe` Right Data.Set.empty
    it "read valid" $ do
      res <- runExceptT $ getPostedEventIDs (createConfig "test/posted_valid.txt")
      res
        `shouldBe` Right
          ( Data.Set.fromList
              [ eventID "334-4-blokove-hry-ls25",
                eventID "333-vyjezdni-zasedani-ls-25",
                eventID "332-velikonocni-party-b3",
                eventID "331-2-blokove-hry-ls25"
              ]
          )
    it "read empty lines" $ do
      res <- runExceptT $ getPostedEventIDs (createConfig "test/posted_empty_lines.txt")
      res `shouldBe` Right (Data.Set.fromList [])
    it "with duplicate files" $ do
      res <- runExceptT $ getPostedEventIDs (createConfig "test/posted_duplicate.txt")
      res
        `shouldBe` Right
          ( Data.Set.fromList
              [ eventID "334-4-blokove-hry-ls25",
                eventID "333-vyjezdni-zasedani-ls-25"
              ]
          )
    it "non existing file" $ do
      res <- runExceptT $ getPostedEventIDs (createConfig "test/non_existing.txt")
      res
        `shouldBe` Right
          (Data.Set.fromList [])
  describe "store pages" $ do
    it "store valid" $ do
      writeFile filename ""
      runExceptT $ storePostedEvent (createConfig filename) (eventID "333-vyjezdni-zasedani-ls-25")
      res <- runExceptT $ getPostedEventIDs (createConfig filename)
      res
        `shouldBe` Right
          ( Data.Set.fromList
              [ eventID "333-vyjezdni-zasedani-ls-25"
              ]
          )
    it "write more" $ do
      runExceptT $ storePostedEvent (createConfig filename) (eventID "334-4-blokove-hry-ls25")
      runExceptT $ storePostedEvent (createConfig filename) (eventID "333-vyjezdni-zasedani-ls-25")
      res <- runExceptT $ getPostedEventIDs (createConfig filename)
      res
        `shouldBe` Right
          ( Data.Set.fromList
              [ eventID "334-4-blokove-hry-ls25",
                eventID "333-vyjezdni-zasedani-ls-25"
              ]
          )
    it "write twice" $ do
      writeFile filename ""
      runExceptT $ storePostedEvent (createConfig filename) (eventID "333-vyjezdni-zasedani-ls-25")
      runExceptT $ storePostedEvent (createConfig filename) (eventID "333-vyjezdni-zasedani-ls-25")
      res <- runExceptT $ getPostedEventIDs (createConfig filename)
      res
        `shouldBe` Right
          ( Data.Set.fromList
              [ eventID "333-vyjezdni-zasedani-ls-25"
              ]
          )
    it "write read random" $ property $ \content -> do
      writeFile filename ""
      forM_ (Data.List.filter (/= "") $ Data.String.lines content) $
        \line -> runExceptT $ storePostedEvent (createConfig filename) (eventID content)
      res <- runExceptT $ getPostedEventIDs (createConfig filename)
      res
        `shouldBe` ( Right
                       . Data.Set.fromList
                       . fmap eventID
                       . Data.List.filter (/= "")
                       . Data.String.lines
                       $ content
                   )
