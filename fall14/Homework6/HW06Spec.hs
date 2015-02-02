{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Test.Hspec
import Data.Aeson
import qualified Data.HashMap.Lazy as Map
import qualified Data.Vector as Vector
import Data.Monoid

import HW06

main :: IO ()
main = hspec $ do
    describe "ynToBool" $ do
        it "converts Ys and Ns to Trues and Falses in a JSON Object" $ do
            let objectWithYn = Object (Map.fromList [("aBool", "Y")])
            let objectWithBool = Object (Map.fromList [("aBool", (Bool True))])
            ynToBool objectWithYn `shouldBe` objectWithBool
        it "converts Ys and Ns to Trues and Falses in a JSON Vector of Objects" $ do
            let objectWithYn = Array $ Vector.fromList [Object $ Map.fromList [("aBool", "Y")]]
            let objectWithBool = Array $ Vector.fromList [Object $ Map.fromList [("aBool", Bool True)]]
            ynToBool objectWithYn `shouldBe` objectWithBool
        it "converts Ys and Ns to Trues and Falses in nested objects" $ do
            let objectWithYn = Object $ Map.fromList [("foo", Object $ Map.fromList [("aBool", "N"), ("aNumber", (Number 42))]), ("bar", "aString")]
            let objectWithBool = Object $ Map.fromList [("foo", Object $ Map.fromList [("aBool", (Bool False)), ("aNumber", (Number 42))]), ("bar", "aString")]
            ynToBool objectWithYn `shouldBe` objectWithBool

    describe "OrdList" $ do
        it "keeps the list ordered" $ do
            OrdList [2,4,6] <> OrdList [1,3,5] `shouldBe` OrdList [1,2,3,4,5,6]
