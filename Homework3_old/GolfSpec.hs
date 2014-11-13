import Test.Hspec
import Golf

main :: IO ()
main = hspec $ do
    describe "Golf.skips" $ do
        it "works" $ do
            skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
            skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
            skips [1] `shouldBe` [[1]]
            skips [True, False] `shouldBe` [[True, False], [False]]
            skips [] `shouldBe` ([] :: [[()]])

    describe "Golf.localMaxima" $ do
        it "works" $ do
            localMaxima [2,9,5,6,1] `shouldBe` [9,6]
            localMaxima [2,3,4,1,5] `shouldBe` [4]
            localMaxima [1,2,3,4,5] `shouldBe` []

    describe "Golf.histogram" $ do
        it "works" $ do
            histogram [] `shouldBe` "==========\n0123456789\n"
            histogram [1, 3, 7] `shouldBe` " * *   *  \n==========\n0123456789\n"
            histogram [1, 1, 1, 5] `shouldBe` " *        \n *        \n *   *    \n==========\n0123456789\n"
            histogram [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"

