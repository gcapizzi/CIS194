import Test.Hspec

import Buffer
import JoinList
import Sized
import Scrabble
import JoinListBuffer

main :: IO ()
main = hspec $ do
    let emptyList = (Empty :: JoinList (Score, Size) String)
    let one = scoreLine "one"
    let two = scoreLine "two"
    let three = scoreLine "three"
    let four = scoreLine "four"
    let five = scoreLine "five"
    let six = scoreLine "six"
    let seven = scoreLine "seven"
    let list = (one +++ (two +++ three)) +++ ((four +++ five) +++ (six +++ seven))

    describe "toString" $ do
        context "with an Empty JoinList" $ do
            it "returns an empty String" $ do
                toString emptyList `shouldBe` ""

        context "with a Single JoinList" $ do
            let singleList = scoreLine "foo"

            it "returns its value" $ do
                toString singleList `shouldBe` "foo"

        context "with an Append JoinList" $ do
            it "joins all the elements with a newline" $ do
                toString list `shouldBe` "one\ntwo\nthree\nfour\nfive\nsix\nseven"

    describe "fromString" $ do
        context "with an empty string" $ do
            it "returns an Empty JoinList" $ do
                (fromString "" :: JoinList (Score, Size) String) `shouldBe` Empty

        context "with a single-line string" $ do
            it "returns a Single JoinList" $ do
                fromString "foo" `shouldBe` Single ((Score 6), (Size 1)) "foo"

        context "with a multi-line string" $ do
            it "creates a balanced tree" $ do
                fromString "one\ntwo\nthree\nfour\nfive\nsix\nseven" `shouldBe` list

    describe "replaceLine" $ do
        context "with an Empty JoinList" $ do
            it "returns an Empty JoinList" $ do
                replaceLine 3 "foo" emptyList `shouldBe` emptyList

        context "with a Single JoinList" $ do
            let foo = scoreLine "foo"

            context "with i = 0" $ do
                it "returns the new value" $ do
                    let bar = scoreLine "bar"
                    replaceLine 0 "bar" foo `shouldBe` bar
            context "with i /= 0" $ do
                it "returns the Single unchanged" $ do
                    replaceLine 1 "bar" foo `shouldBe` foo

        context "with an Append JoinList" $ do
            it "replaces the line" $ do
                let newThree = scoreLine "THREE"
                toList (replaceLine 2 "THREE" list) `shouldBe` ["one", "two", "THREE", "four", "five", "six", "seven"]
