module Party where

import Employee
import Data.Monoid

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL employees fun) = GL (employee:employees) (fun + empFun employee)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL xs xfun) (GL ys yfun) = GL (xs ++ ys) (xfun + yfun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun xs@(GL _ xfun) ys@(GL _ yfun) | xfun >= yfun = xs
moreFun _ ys = ys
