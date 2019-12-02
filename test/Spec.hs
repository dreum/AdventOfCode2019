import Test.Hspec
import Day1

main :: IO ()
main = hspec $ do 
    describe "Day1 tests" $ do
        it "calculates fuel required" $ do
            fuelRequired 12 `shouldBe` (2 :: Integer)
