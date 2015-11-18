import Test.Hspec
import Pt
import Pt2
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "pt" $ do
        it "returns 1 when given 0 0" $ do
            pt 0 0 `shouldBe` (1 :: Int)
    
        it "returns 1 when given 1 0" $ do
            pt 1 0 `shouldBe` (1 :: Int)

        it "returns 2 when given 2 1" $ do
            pt 2 1 `shouldBe` (2 :: Int)

        it "returns 4 when given 4 1" $ do
            pt 4 1 `shouldBe` (4 :: Int)
    
    describe "ptRow" $ do
        it "returns [1] when given 0" $ do
            ptRow 0 `shouldBe` ([1])
            
    describe "pt2OutLines" $ do
        it "returns [\"1\"] when given 1" $ do
            pt2OutLines 1 `shouldBe` (["1"])