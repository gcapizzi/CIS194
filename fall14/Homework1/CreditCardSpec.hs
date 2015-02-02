import Test.Hspec
import CreditCard

main :: IO ()
main = hspec $ do
    describe "CreditCard.toDigits" $ do
        it "converts a positive Integer to a List of its digits" $ do
            toDigits 0 `shouldBe` []
            toDigits 3 `shouldBe` [3]
            toDigits 12345 `shouldBe` [1, 2, 3, 4, 5]

    describe "CreditCard.toDigitsRev" $ do
        it "converts a positive Integer to a reverse list of its digits" $ do
            toDigitsRev 12345 `shouldBe` [5, 4, 3, 2, 1]

    describe "CreditCard.doubleEveryOther" $ do
        it "doubles every other number of a list, beginning from the right" $ do
            doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]

    describe "CreditCard.sumDigits" $ do
        it "sums all digits of all numbers in a list" $ do
            sumDigits [16,7,12,5] `shouldBe` 22

    describe "CreditCard.validate" $ do
        it " validates a credit card number" $ do
            validate 4012888888881881 `shouldBe` True
            validate 4012888888881882 `shouldBe` False
