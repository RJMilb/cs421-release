--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Metadata for autograder
--- -----------------------
tag1 = 21923
tag2 = 44437
tag3 = 24929

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake n (x:xs) 
        | n < 1 = []
        | otherwise = x : mytake (n-1) xs


--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n (x:xs) 
        | n < 1 = x : xs
        | otherwise = mydrop (n-1) xs
-- mydrop = undefined

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev xs = revApp xs []
    where 
        revApp :: [a] -> [a] -> [a]
        revApp [] r = r
        revApp (x:xs) r = revApp xs ( x:r)
-- rev = undefined


--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app xs [] = xs
app [] ys = ys
app (x:xs) ys = x : app xs ys
-- app = undefined

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1) : inclist xs
-- inclist = undefined

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs
-- sumlist = undefined

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip [] r = []
myzip l [] = []
myzip (x:xs) (y:ys) = (x, y) : myzip xs ys
-- myzip = undefined

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs l r = pairApp (myzip l r)
    where
        pairApp :: (Num b) => [(b,b)] -> [b]
        pairApp [] = []
        pairApp ((x,y):xs) = (x+y) : pairApp xs 
-- addpairs = undefined

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = [1, 1..]
-- ones = undefined

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add n [] = [n]
add n (x:xs)
    | n < x  = n : x : xs
    | n == x = x : xs
    | otherwise = x : add n xs 
-- add = undefined

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union xs [] = xs
union [] ys = ys
union (x:xs) (y:ys)
    | x == y = x : union xs ys
    | x < y  = x : union xs (y:ys)
    | x > y  = y : union (x:xs) ys
-- union = undefined

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect xs [] = []
intersect [] ys = []
intersect (x:xs) (y:ys)
    | x == y = x : intersect xs ys
    | x < y  = intersect xs (y:ys)
    | x > y  = intersect (x:xs) ys
-- intersect = undefined

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union pow (powerApp x pow)
    where
        pow = powerset xs

        powerApp _ [] = []
        powerApp n (y:ys) = add (add n y) (powerApp n ys)

-- powerset = undefined

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' x = P.map (+1) x
-- inclist' = undefined

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' x = sum x
-- sumlist' = undefined
