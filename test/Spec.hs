import Test.Hspec
import Day1

main :: IO ()
main = hspec $ do 
    describe "Day1 tests" $ do
        it "calculates fuel required 1" $ do
            fuelRequired 12 `shouldBe` (2 :: Integer)
        it "calculates fuel required 2" $ do
            fuelRequired 14 `shouldBe` (2 :: Integer)
        it "calculates fuel required 3" $ do
            fuelRequired 1969 `shouldBe` (654 :: Integer)
        it "calculates fuel required 4" $ do
            fuelRequired 100756 `shouldBe` (33583 :: Integer)
