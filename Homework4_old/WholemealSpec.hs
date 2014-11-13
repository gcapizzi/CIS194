import Test.Hspec
import Wholemeal

main :: IO ()
main = hspec $ do
    describe "Wholemeal.fun1" $ do
        it "takes all even numbers, subtracts 2 and then multiplies them" $ do
            fun1 [1,2,3,4,5,6] `shouldBe` 0
            fun1 [3,4,5,6,7,8,9] `shouldBe` 48

    describe "Wholemeal.fun2" $ do
        it "does some weird things" $ do
            fun2 1 `shouldBe` 0
            fun2 4 `shouldBe` 4 + 2
            fun2 3 `shouldBe` 10 + 16 + 8 + 4 + 2

    describe "Wholemeal.xor" $ do
        it "returns True if and only if there are an odd number of True" $ do
            xor [False, True, False] `shouldBe` True
            xor [False, True, False, False, True] `shouldBe` False

    describe "Wholemeal.map'" $ do
        it "behaves just like map" $ do
            map' (+ 1) [] `shouldBe` []
            map' (+ 1) [1, 2, 3] `shouldBe` [2, 3, 4]

    describe "Wholemeal.sieveSundaram" $ do
        it "computes all primes below (2*n)+2" $ do
            sieveSundaram 10 `shouldBe` [3, 5, 7, 11, 13, 17, 19]
