module Party where

import Employee
import Data.Monoid
import Data.Tree
import Debug.Trace

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL employees fun) = GL (employee:employees) (fun + empFun employee)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL xs xfun) (GL ys yfun) = GL (xs ++ ys) (xfun + yfun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun xs@(GL _ xfun) ys@(GL _ yfun)
  | xfun >= yfun = xs
  | otherwise    = ys

treeFold :: (Show a, Show b) => ([b] -> a -> b) -> b -> Tree a -> b
treeFold f z (Node x []) = f [z] x
treeFold f z (Node x subTrees) = f (map (treeFold f z) subTrees) x
