import Test.Hspec

import Ring
import Parser

import HW05

main :: IO ()
main = hspec $ do
  describe "Mod5" $ do
    it "supports addition" $ do
      add (Mod5 1) (Mod5 2) `shouldBe` (Mod5 3)
      add (Mod5 3) (Mod5 2) `shouldBe` (Mod5 0)
    it "uses 0 as the additive identity" $ do
      add (Mod5 3) addId `shouldBe` (Mod5 3)
    it "uses negation as the additive inverse" $ do
      add (Mod5 3) (addInv (Mod5 3)) `shouldBe` addId
    it "supports multiplication" $ do
      mul (Mod5 2) (Mod5 2) `shouldBe` (Mod5 4)
      mul (Mod5 2) (Mod5 3) `shouldBe` (Mod5 1)
    it "uses 1 as the multiplicative identity" $ do
      mul (Mod5 2) mulId `shouldBe` (Mod5 2)
    it "is parseable" $ do
      parseRing "1 + 2 * 5" `shouldBe` Just (Mod5 1)

  describe "Mat2x2" $ do
    it "supports addition" $ do
      add (Mat2x2 1 2 3 4) (Mat2x2 5 6 7 8) `shouldBe` (Mat2x2 6 8 10 12)
    it "uses [[0,0][0,0]] as the additive identity" $ do
      add (Mat2x2 1 2 3 4) addId `shouldBe` (Mat2x2 1 2 3 4)
    it "uses negation as the additive inverse" $ do
      add (Mat2x2 1 2 3 4) (addInv (Mat2x2 1 2 3 4)) `shouldBe` addId
    it "supports multiplication" $ do
      mul (Mat2x2 1 2 3 4) (Mat2x2 5 6 7 8) `shouldBe` (Mat2x2 19 22 43 50)
    it "uses [[1,0][0,1]] as the multiplicative identity" $ do
      mul (Mat2x2 1 2 3 4) mulId `shouldBe` (Mat2x2 1 2 3 4)
    it "is parseable" $ do
      parse "[[1,2][3,4]] *" `shouldBe` Just ((Mat2x2 1 2 3 4), " *")
      parseRing "[[1,2][3,4]] + [[5,6][7,8]]" `shouldBe` Just (Mat2x2 6 8 10 12)

  describe "Bool" $ do
    it "supports addition" $ do
      add True False `shouldBe` True
    it "uses False as the additive identity" $ do
      add True addId `shouldBe` True
      add False addId `shouldBe` False
    it "uses negation as the additive inverse" $ do
      add True (addInv True) `shouldBe` addId
      add False (addInv False) `shouldBe` addId
    it "supports multiplication" $ do
      mul True True `shouldBe` True
      mul True False `shouldBe` False
      mul False True `shouldBe` False
      mul False False `shouldBe` False
    it "uses True as the multiplicative identity" $ do
      mul True mulId `shouldBe` True
      mul False mulId `shouldBe` False
    it "is parseable" $ do
      parseRing "True + False * True" `shouldBe` Just True
