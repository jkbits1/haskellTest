import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Zip

main :: IO ()
main = hspec $ do
    describe "zip" $ do
        it "returns \"axbycz\" when given \"abc\" \"xyx\"" $ do
            zipp "abc" "xyz" `shouldBe` ("axbycz" :: [Char])
            

    
