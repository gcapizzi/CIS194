import Test.Hspec

import Employee
import Party

import Data.Tree

main :: IO ()
main = hspec $ do
  describe "GuestList" $ do
    let alice = Emp {empName="Alice", empFun=1}
    let bob = Emp {empName="Bob", empFun=2}
    let carol = Emp {empName="Carol", empFun=3}

    it "is a Monoid" $ do
      mappend mempty (GL [alice, bob] 3) `shouldBe` GL [alice, bob] 3
      mappend (GL [alice, bob] 3) (GL [carol] 3) `shouldBe` GL [alice, bob, carol] 6

    describe "glCons" $ do
      context "with an empty GuestList" $ do
        it "returns a GuestList with one Employee" $ do
          glCons alice (GL [] 0) `shouldBe` GL [alice] 1

      context "with a non-empty GuestList" $ do
        it "adds the employee to the list" $ do
          glCons carol (GL [alice, bob] 3) `shouldBe` GL [alice, bob, carol] 6

    describe "moreFun" $ do
      it "returns the list with more fun" $ do
        moreFun (GL [] 3) (GL [] 1) `shouldBe` True
        moreFun (GL [] 1) (GL [] 2) `shouldBe` False

  describe "treeFold" $ do
    it "folds a Tree" $ do
      let tree = Node 1 [Node 2 [Node 3 [], Node 4 [], Node 5 []], Node 6 [], Node 7 [Node 8 [], Node 9 []]] :: Tree Int
      let strCat xs x = show x ++ concat xs

      treeFold "" strCat tree `shouldBe` "123456789"
