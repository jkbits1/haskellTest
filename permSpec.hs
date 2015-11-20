import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Perm

main :: IO ()
main = hspec $ do
    describe "strPerm" $ do
        it "returns \"ba\" when given \"ab\" " $ do
            strPerm "ab" `shouldBe` ("ba" :: [Char])

        it "returns \"zy\" when given \"yz\" " $ do
            strPerm "yz" `shouldBe` ("zy" :: [Char])

        it "returns \"bazy\" when given \"abyz\" " $ do
            strPerm "abyz" `shouldBe` ("bazy" :: [Char])


    
