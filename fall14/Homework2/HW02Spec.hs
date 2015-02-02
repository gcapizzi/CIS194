import Test.Hspec
import HW02

main :: IO ()
main = hspec $ do
    describe "formableBy" $ do
        it "tells if a word is formable with a hand" $ do
            formableBy "fun" ['x', 'n', 'i', 'f', 'u', 'e', 'l'] `shouldBe` True
            formableBy "fun" ['k', 'l', 'e', 'h', 'a', 'y', 's'] `shouldBe` False
    describe "wordsFrom" $ do
        it "finds all words formable with a hand" $ do
            wordsFrom ['h', 'e', 'l', 'l', 'o'] `shouldBe` ["eh", "el", "ell", "he", "hell", "hello", "helo",  "ho", "hoe", "hole", "lo", "oe", "oh", "ole"]
    describe "wordFitsTemplate" $ do
        it "tells if a word fits a template" $ do
            wordFitsTemplate "?" ['a'] "b" `shouldBe` False
            wordFitsTemplate "?" ['a'] "a" `shouldBe` True
            wordFitsTemplate "a" [] "a" `shouldBe` True
            wordFitsTemplate "a?" ['c'] "ab" `shouldBe` False
            wordFitsTemplate "?b" ['a'] "ac" `shouldBe` False
            wordFitsTemplate "??r?" ['c', 'x', 'e', 'a', 'b', 'c', 'l'] "care" `shouldBe` True
            wordFitsTemplate "??r?" ['c', 'x', 'e', 'w', 'b', 'c', 'l'] "care" `shouldBe` False
            wordFitsTemplate "??r?" ['c', 'x', 'e', 'a', 'b', 'c', 'l'] "car" `shouldBe` False
            wordFitsTemplate "let" ['x', 'x'] "let" `shouldBe` True
    describe "wordsFittingTemplate" $ do
        it "finds all words fitting a template" $ do
            wordsFittingTemplate "??r?" ['c', 'x', 'e', 'a', 'b', 'c', 'l'] `shouldBe` ["acre", "bare", "carb", "care", "carl", "earl"]
    describe "scrabbleValueWord" $ do
        it "gives the point value of a word" $ do
            scrabbleValueWord "care" `shouldBe` 6
            scrabbleValueWord "quiz" `shouldBe` 22
    describe "bestWords" $ do
        it "returns a list of words with the maximum point value" $ do
            bestWords ["acre", "bare", "carb", "care", "carl", "earl"] `shouldBe` ["carb"]
            bestWords ["cat", "rat", "bat"] `shouldBe` ["bat", "cat"]
            bestWords [] `shouldBe` []
    describe "scrabbleValueTemplate" $ do
        it "computes the value of playing a given word on a given template" $ do
            scrabbleValueTemplate "?e??3" "peace" `shouldBe` 27
