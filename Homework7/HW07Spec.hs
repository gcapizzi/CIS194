import Test.Hspec

import HW07

main :: IO ()
main = hspec $ do
    describe "fib" $ do
        it "computes the Fibonacci function" $ do
            fib 0 `shouldBe` 0
            fib 1 `shouldBe` 1
            fib 9 `shouldBe` 34
    describe "fibs1" $ do
        it "computes the infinite list of Fibonacci numbers" $ do
            take 15 fibs1 `shouldBe` [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377]
    describe "fibs2" $ do
        it "computes the infinite list of Fibonacci numbers" $ do
            take 15 fibs2 `shouldBe` [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377]
    describe "Stream" $ do
        describe "show" $ do
            it "shows the first 20 elements" $ do
                show (streamRepeat 1) `shouldBe` "[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]"
        describe "streamMap" $ do
            it "maps a function on a Stream" $ do
                show (streamMap (+1) (streamRepeat 1)) `shouldBe`  "[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]"
        describe "streamFromSeed" $ do
            it "generates a Stream from a seed and an unfolding rule" $ do
                show (streamFromSeed (+1) 1) `shouldBe` "[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]"
