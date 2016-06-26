module Party where

import Data.Tree

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL list fun) = GL (list ++ [employee]) (fun + empFun employee)

moreFun (GL _ funOne) (GL _ funTwo) = funOne > funTwo

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL listOne funOne) (GL listTwo funTwo) = GL (listOne ++ listTwo) (funOne + funTwo)

treeFold :: b -> ([b] -> a -> b) -> Tree a -> b
treeFold z f (Node x ts) = f (map (treeFold z f) ts) x
