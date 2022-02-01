{-# OPTIONS_GHC -Wall -Wno-type-defaults -fno-warn-missing-signatures #-}

module Examples where

import Prelude hiding (length, map, take, drop, sum, foldr, foldl)

--
-- List examples
--

-- Compute the length of the list l
length = error "length unimplemented"

-- Compute the sum of a list of integers
sum = error "sum unimplemented"

-- Return the nth element of a list, counting from 0.
nth = error "nth unimplemented"

-- Append two lists
append = error "append unimplemented"

-- Take the first n elements of a list
take = error "take unimplemented"

-- Drop the first n elements of a list
drop = error "drop unimplemented"

--
-- Partial application
--

-- Add two integers
add :: Int -> Int -> Int
add x y = x + y

-- Increment an integer
inc :: Int -> Int
inc = error "inc unimplemented"

--
-- Higher-order functions
--

-- Increment all elements of a list by 1
incAll = error "incAll unimplemented"

-- Increment all elements of a list by a constant
addAll = error "addAll unimplemented"

map = error "map unimplemented"

-- Calculate the squares of a list of numbers. Make the function non-recursive.
squares = error "squares unimplemented"

-- Now write squares using a list comprehension. We give this variant the name squares'
squares' = error "squares' unimplemented"

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

