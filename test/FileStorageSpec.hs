module FileStorageSpec where

import Data.Set (empty, fromList)
import FileStorage (createConfig, getPostedEvents, storePostedEvent)
import Model (EventID (..))
import System.IO
import Test.Hspec

spec :: Spec
spec = do
  describe "valid pages" $ do
    it "read empty" $ do
      res <- getPostedEvents (createConfig "test/posted_emtpy.txt")
      res `shouldBe` Right Data.Set.empty
    it "read valid" $ do
      res <- getPostedEvents (createConfig "test/posted_valid.txt")
      res
        `shouldBe` Right
          ( Data.Set.fromList
              [ EventID "334-4-blokove-hry-ls25",
                EventID "333-vyjezdni-zasedani-ls-25",
                EventID "332-velikonocni-party-b3",
                EventID "331-2-blokove-hry-ls25"
              ]
          )
    it "read empty lines" $ do
      res <- getPostedEvents (createConfig "test/posted_empty_lines.txt")
      res `shouldBe` Right (Data.Set.fromList [])
    it "with duplicate files" $ do
      res <- getPostedEvents (createConfig "test/posted_duplicate.txt")
      res
        `shouldBe` Right
          ( Data.Set.fromList
              [ EventID "334-4-blokove-hry-ls25",
                EventID "333-vyjezdni-zasedani-ls-25"
              ]
          )
    it "non existing file" $ do
      res <- getPostedEvents (createConfig "test/non_existing.txt")
      res
        `shouldBe` Right
          (Data.Set.fromList [])
  describe "store pages" $ do
    it "store valid" $ do
      let filename = "test/store.txt"
      writeFile filename ""
      storePostedEvent (createConfig filename) (EventID "333-vyjezdni-zasedani-ls-25")
      res <- getPostedEvents (createConfig filename)
      res
        `shouldBe` Right
          ( Data.Set.fromList
              [ EventID "333-vyjezdni-zasedani-ls-25"
              ]
          )
    it "write more" $ do
      let filename = "test/store.txt"
      writeFile filename ""
      storePostedEvent (createConfig filename) (EventID "334-4-blokove-hry-ls25")
      storePostedEvent (createConfig filename) (EventID "333-vyjezdni-zasedani-ls-25")
      res <- getPostedEvents (createConfig filename)
      res
        `shouldBe` Right
          ( Data.Set.fromList
              [ EventID "334-4-blokove-hry-ls25",
                EventID "333-vyjezdni-zasedani-ls-25"
              ]
          )
    it "write twice" $ do
      let filename = "test/store.txt"
      writeFile filename ""
      storePostedEvent (createConfig filename) (EventID "333-vyjezdni-zasedani-ls-25")
      storePostedEvent (createConfig filename) (EventID "333-vyjezdni-zasedani-ls-25")
      res <- getPostedEvents (createConfig filename)
      res
        `shouldBe` Right
          ( Data.Set.fromList
              [ EventID "333-vyjezdni-zasedani-ls-25"
              ]
          )
