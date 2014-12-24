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
