import Test.Hspec
import ExprT
import Calc

main :: IO ()
main = hspec $ do
    describe "Calc.eval" $ do
        it "evals literals" $ do
            eval (Lit 4) `shouldBe` 4

        it "evals sums" $ do
            eval (Add (Lit 2) (Lit 3)) `shouldBe` 5

        it "evals products" $ do
            eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

    describe "Calc.evalStr" $ do
        it "evals a string expression" $ do
            evalStr "1+" `shouldBe` Nothing
            evalStr "2+3*4" `shouldBe` Just 14

    describe "Calc.Expr" $ do
        it "provides a DSL" $ do
            eval (mul (add (lit 2) (lit 3)) (lit 4)) `shouldBe` 20

    -- describe "Integer as an Expr instance" $ do
    --     it "works" $ do


