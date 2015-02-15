import Test.Hspec

import JoinList
import Sized
import Debug.Trace
import Scrabble

main :: IO ()
main = hspec $ do
    let emptyList = Empty :: JoinList Size String
    let single x = Single (1 :: Size) x

    let a = single "a"
    let b = single "b"
    let c = single "c"
    let d = single "d"
    let e = single "e"
    let f = single "f"
    let g = single "g"

    let alphaList = a +++ (b +++ c) +++ (d +++ e +++ (f +++ g))

    describe "+++" $ do
        it "appends two JoinLists" $ do
            let one   = Single "one"   1
            let two   = Single "two"   2
            let three = Single "three" 3

            let sum = Empty +++ one +++ (two +++ three) +++ Empty

            sum `shouldBe` (Append "onetwothree" one (Append "twothree" two three))

    describe "indexJ" $ do
        context "with an Empty JoinList" $ do
            it "returns Nothing" $ do
                (indexJ 3 emptyList :: Maybe String) `shouldBe` Nothing

        context "with a Single JoinList" $ do
            context "with a 0 index" $ do
                it "returns the element" $ do
                    indexJ 0 (single "foo")  `shouldBe` Just "foo"

            context "with an index /= 0" $ do
                it "returns Nothing" $ do
                    indexJ (-3) (single "foo")  `shouldBe` Nothing
                    indexJ 3 (single "foo")  `shouldBe` Nothing

        context "with an Append JoinList" $ do
            it "returns the nth element" $ do
                indexJ (-1) alphaList `shouldBe` Nothing
                indexJ 0 alphaList `shouldBe` Just "a"
                indexJ 1 alphaList `shouldBe` Just "b"
                indexJ 2 alphaList `shouldBe` Just "c"
                indexJ 3 alphaList `shouldBe` Just "d"
                indexJ 4 alphaList `shouldBe` Just "e"
                indexJ 5 alphaList `shouldBe` Just "f"
                indexJ 6 alphaList `shouldBe` Just "g"
                indexJ 7 alphaList `shouldBe` Nothing

    describe "toList" $ do
        it "turns a JoinList into a regular list" $ do
            toList emptyList `shouldBe` []
            toList alphaList `shouldBe` ["a", "b", "c", "d", "e", "f", "g"]

    describe "dropJ" $ do
        context "with an Empty JoinList" $ do
            it "returns an Empty JoinList" $ do
                dropJ 3 emptyList `shouldBe` Empty

        context "with a Single JoinList" $ do
            context "with n <= 0" $ do
                it "returns the list itself" $ do
                    toList (dropJ 0 (single "foo")) `shouldBe` ["foo"]
                    toList (dropJ (-3) (single "foo")) `shouldBe` ["foo"]

            context "with n > 0" $ do
                it "returns an Empty JoinList" $ do
                    dropJ 3 (single "foo") `shouldBe` Empty

        context "with an Append JoinList" $ do
            context "with n <= 0" $ do
                it "returns the list itself" $ do
                    dropJ 0 alphaList `shouldBe` alphaList
                    dropJ (-3) alphaList `shouldBe` alphaList

            context "with n > size" $ do
                it "returns an Empty JoinList" $ do
                    toList (dropJ 8 alphaList) `shouldBe` []

            context "with 0 < n < size" $ do
                it "returns the remaining elements" $ do
                    toList (dropJ 4 alphaList) `shouldBe` ["e", "f", "g"]

    describe "takeJ" $ do
        context "with an Empty JoinList" $ do
            it "returns an Empty JoinList" $ do
                takeJ 3 emptyList `shouldBe` Empty

        context "with a Single JoinList" $ do
            context "with n <= 0" $ do
                it "returns an Empty JoinList" $ do
                    takeJ 0 (single "foo") `shouldBe` Empty
                    takeJ (-3) (single "foo") `shouldBe` Empty

            context "with n > 0" $ do
                it "returns the list itself" $ do
                    toList (takeJ 1 (single "foo")) `shouldBe` ["foo"]
                    toList (takeJ 3 (single "foo")) `shouldBe` ["foo"]

        context "with an Append JoinList" $ do
            context "with n <= 0" $ do
                it "returns the Empty JoinList" $ do
                    takeJ 0 alphaList `shouldBe` Empty
                    takeJ (-3) alphaList `shouldBe` Empty

            context "with n > size" $ do
                it "returns an Empty JoinList" $ do
                    takeJ 8 alphaList `shouldBe` alphaList

            context "with 0 < n < size" $ do
                it "returns the first n elements" $ do
                    toList (takeJ 4 alphaList) `shouldBe` ["a", "b", "c", "d"]

    describe "scoreLine" $ do
        it "returns the line as a Single JoinList with Score" $ do
            scoreLine "yay " `shouldBe` Single ((Score 9), (Size 4)) "yay "
            scoreLine "haskell!" `shouldBe` Single ((Score 14), (Size 8)) "haskell!"
