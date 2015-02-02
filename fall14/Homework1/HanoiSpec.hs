import Test.Hspec
import Hanoi

main :: IO ()
main = hspec $ do
    describe "Hanoi.hanoi" $ do
        it "moves a disc from the a to b" $ do
            hanoi 1 "a" "b" "c" `shouldBe` [("a", "b")]

        it "moves n discs from a to b using c as temporary storage" $ do
            hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"),
                                            ("a", "b"),
                                            ("c", "b")]

            hanoi 3 "a" "b" "c" `shouldBe` [("a", "b"),
                                            ("a", "c"),
                                            ("b", "c"),
                                            ("a", "b"),
                                            ("c", "a"),
                                            ("c", "b"),
                                            ("a", "b")]
