{-# OPTIONS_GHC -Wall -Wno-type-defaults -fno-warn-missing-signatures #-}

module Examples where

import Prelude hiding (length, map, take, drop, sum, foldr, foldl)

--
-- List examples
--

-- Compute the length of the list l
length :: [a] -> Int
--length xs = if null xs then 0 else 1 + length (tail xs)
length []     = 0
length (_:xs) = 1 + length xs

-- Compute the sum of a list of integers
sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

-- Return the nth element of a list, counting from 0.
nth :: Int -> [a] -> a
nth _ []     = error "nth: not enough elements"
nth 0 (x:_)  = x
nth n (_:xs) = nth (n-1) xs

-- Append two lists
append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

-- Take the first n elements of a list
take :: Int -> [a] -> [a]
take 0 _      = []
take _ []     = []
take n (x:xs) = x : take (n-1) xs

-- Drop the first n elements of a list
drop :: Int -> [a] -> [a]
drop 0 xs     = xs
drop _ []     = []
drop n (_:xs) = drop (n-1) xs

--
-- Partial application
--

-- Add two integers
add2 :: Int -> Int -> Int
add2 x y = x + y

-- Increment an integer
inc :: Int -> Int
inc = error "inc unimplemented"

--
-- Recursive Types (PIH 8.4)
--

data Nat = Zero | Succ Nat

nat2int :: Nat -> Integer
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Integer -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

--
-- Higher-order functions
--

-- Increment all elements of a list by 1
incAll = error "incAll unimplemented"

-- Increment all elements of a list by a constant
addAll = error "addAll unimplemented"

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- Calculate the squares of a list of numbers. Make the function non-recursive.
squares :: Num a => [a] -> [a]
squares xs = map (\x -> x*x) xs

-- Now write squares using a list comprehension. We give this variant the name squares'
squares' :: Num a => [a] -> [a]
squares' xs = [x*x | x <- xs]

curry = error "curry unimplemented"

uncurry = error "uncurry unimplemented"

(.)  = error "(.) unimplemented"

($)  = error "($) unimplemented"

-- What about papply?

--
-- Folds
--
foldr = error "foldr unimplemented"

foldl = error "foldl unimplemented"

