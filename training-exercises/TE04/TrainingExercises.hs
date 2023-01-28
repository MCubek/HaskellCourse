-- =============================================================================== --

-- =============================================================================== --
--

-- |
--  Welcome to your fourth Haskell training. Get ready to rumble.
--  Where will you let recursion lead you?
--
--  You should know the drill by now - solve the exercises in `TrainingExercises.hs`,
--  run the tests with Cabal, push to `training-04`, create a Merge Request,
--  and assign it to your TA.
--
--  Keep in mind that **all exposed functions must have type signatures** (helper functions that are defined in local definitions don't)!
--
--  As always, ask your TA if you need any help.
module TrainingExercises where

--

import Data.Char
import Data.List

--

-- * 4.1 Recursive functions

-- ** TE 4.1.1

--

-- | Define a recursive function that calculates the minimal number of moves needed to complete a game
-- | of Towers of Hanoi with n disks.
-- |
-- | In case you don't know about that game, take a look here:
-- | https://en.wikipedia.org/wiki/Tower_of_Hanoi#Recursive_solution
te411 :: (Eq t, Num t) => t -> t
te411 1 = 1
te411 n = 1 + 2 * te411 (n - 1)

-- ** TE 4.1.2

--

-- | Define a recursive function that calculates the greatest common divisor of two given numbers.
te412 :: (Integral a) => a -> a -> a
te412 _ 0 = 1
te412 a b
  | a == b = a
  | otherwise = te412 (larger - smaller) smaller
  where
    smaller = min a b
    larger = max a b

-- ** TE 4.1.3

--

-- | Define a recursive function that returns the last element of a list.
-- | What do you think should happen with an empty list?
te413 :: [a] -> a
te413 [] = error "Empty list"
te413 [x] = x
te413 (x : xs) = te413 xs

-- ** TE 4.1.4

--

-- | You have seen a Quick Sort implementation on the lecture. Now is the time to implement Merge Sort.
-- | You are not allowed to use list comprehension here!
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

part :: [a] -> ([a], [a])
part l = splitAt x l
  where
    x = length l `div` 2

te414 :: Ord a => [a] -> [a]
te414 [] = []
te414 [x] = [x]
te414 xs = merge (te414 left) (te414 right)
  where
    (left, right) = part xs

-- ** TE 4.1.5 - EXTRA

--

-- | Now you have written 2 different efficient sorting algorithms. Let's write something worse!
-- | Write an Insertion sort function.
-- | List comprehensions are not alowed once again!
te415 :: Ord a => [a] -> [a]
te415 [] = []
te415 [a] = [a]
te415 (a : as) = insert a (te415 as)
  where
    insert x [] = [x]
    insert x (y : ys)
      | x < y = x : y : ys
      | otherwise = y : insert x ys

-- \* 4.2 Corecursion

-- ** TE 4.2.1

--

-- | Write your own definition of the cycle function.
te421 :: [a] -> [a]
te421 [] = []
te421 xs = xs ++ te421 xs
