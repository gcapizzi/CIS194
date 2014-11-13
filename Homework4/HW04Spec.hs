import Test.Hspec
import HW04

main :: IO ()
main = hspec $ do
    describe "insertBST" $ do
        context "when the tree is empty" $ do
            it "returns a one-element tree" $ do
                insertBST compare 42 Leaf `shouldBe` Node Leaf 42 Leaf

        context "when the inserted element is less than the root of the tree" $ do
            it "inserts the element in the left sub-tree" $ do
                insertBST compare 32 (Node Leaf 42 Leaf) `shouldBe` (Node (Node Leaf 32 Leaf) 42 Leaf)

        context "when the inserted element is equal to the root of the tree" $ do
            it "inserts the element in the right sub-tree" $ do
                insertBST compare 42 (Node Leaf 42 Leaf) `shouldBe` (Node Leaf 42 (Node Leaf 42 Leaf))

        context "when the inserted element is greater than the root of the tree" $ do
            it "inserts the element in the right sub-tree" $ do
                insertBST compare 52 (Node Leaf 42 Leaf) `shouldBe` (Node Leaf 42 (Node Leaf 52 Leaf))

    describe "allCaps" $ do
        it "returns True if a list of strings contains only capitalized words" $ do
            allCaps [] `shouldBe` True
            allCaps ["Hi","There"] `shouldBe` True
            allCaps ["", "Blah"] `shouldBe` False
            allCaps ["Hi","there"] `shouldBe` False

    describe "dropTrailingWhitespace" $ do
        it "drops trailing whitespace from a string" $ do
            dropTrailingWhitespace "foo" `shouldBe` "foo"
            dropTrailingWhitespace "" `shouldBe` ""
            dropTrailingWhitespace "bar " `shouldBe` "bar"

    describe "firstLetters" $ do
        it "gets the first letter of a list of strings" $ do
            firstLetters ["foo", "bar"] `shouldBe` ['f', 'b']
            firstLetters ["alpha",""] `shouldBe` ['a']
            firstLetters [] `shouldBe` []
            firstLetters ["",""] `shouldBe` []

    describe "asList" $ do
        it "Render a proper bracketed list given a list of strings" $ do
            asList ["alpha", "beta", "gamma"] `shouldBe` "[alpha,beta,gamma]"
            asList [] `shouldBe` "[]"
            asList ["lonely"] `shouldBe` "[lonely]"
