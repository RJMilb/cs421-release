--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

--- Metadata for autograder
--- -----------------------
tag1 = 21923
tag2 = 44437
tag3 = 24929

--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)
-- list2cons = undefined

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x xs) = x : cons2list xs
-- cons2list = undefined

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp i) = i
eval (PlusExp []) = 0
eval (PlusExp px) = plusApp px
  where 
    plusApp :: [Exp] -> Integer
    plusApp (x:xs) = (eval x) + (plusApp xs)
    plusApp [] = 0
eval (MultExp []) = 1 
eval (MultExp mx) = multApp mx
  where 
    multApp :: [Exp] -> Integer
    multApp (x:xs) = (eval x) * (multApp xs)
    multApp [] = 1
-- eval = undefined

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' = foldr Cons Nil
-- list2cons' = undefined

--- ### BinTree

-- BinTree
data BinTree a = Node a (BinTree a) (BinTree a)
               | Leaf
  deriving (Show)

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node n left right) = n + (sumTree left) + (sumTree right)
-- sumTree = undefined

--- ### SimpVal

-- SimpVal
data SimpVal = IntVal Integer
               | BoolVal Bool
               | StrVal String
               | ExnVal String 
  deriving (Show)

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp op (IntVal x) (IntVal y) = IntVal (op x y)
liftIntOp _ _ _ = ExnVal "not an IntVal!"
-- liftIntOp = undefined
