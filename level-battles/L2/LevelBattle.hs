-- ========================================================================== --

-- ========================================================================== --

-- |
-- # PUH Level 2 Battle
--
--   Welcome to your second PUH Battle!
--
--   Unlike training, you're alone here. But don't fret - if you solved all your
--   exercises and have a good understanding of topics covered, you should have
--   no problems.
--
--   This battle consists of 4 tasks. **You must successfully solve all of them in
--   order to win the battle.** Every task has a single function in the form of
--   `l2x`, e.g. for the first task you have to write a function `l21`.
--
--   The output will be tested both automatically and manually, which means you must
--   take care of the output formatting (spaces, punctuation etc.)
--
--   **Do not change provided type signatures!** They're here to help you. Use them
--   wisely.
--
--   Finally, you are allowed (encouraged, even) to **create helper functions** as
--   you see fit. However, every helper function **must** have type signatures
--   defined.
--
--   If you want two takes at this battle, submit the first one (this one) before the
--   soft deadline.
--
--   You can use cabal or GHCi.
--
--   To use cabal, run
--
--   >> cabal repl
--
--   which will run repl. To reload, type `:r`.
--
--   If that's not working for you, run
--
--   >> ghci
--
--   and load the Level Battle:
--
--   >> :l LevelBattle
--
--   Although you are not allowed to ask for help in solving the exercises, if you
--   have any trouble with infrastructure, running repl, or anything similar,
--   **please ask for help on Discord** (but do it in public channels).
--
--   Good luck!
module LevelBattle where

--

import Data.Char
import Data.List

--

-- * L 2.1

--

-- | Write a recursive function which removes consecutive repeated elements from a list.
-- Example:
-- lb21 "aaabcccbdda" = "abcbda"
lb21 :: Eq a => [a] -> [a]
lb21 [] = []
lb21 (x1 : x2 : xs)
    | x1 == x2 = lb21 (x2:xs)
    | otherwise = x1 : lb21 (x2:xs)
lb21 (x) = x

-- ** L 2.2

--

-- | Implement a tail recursive function which returns the first index of
-- the given element a list. It should return -1 if the element is not in the list.
-- Example:
-- lb22 3 [1, 3, 5, 1, 3] = 1
lb22 :: Eq a => a -> [a] -> Int
lb22 element ls = indexFinder element ls 0
    where
    indexFinder _ [] _ = -1
    indexFinder el (x : xs) idx
      | x == el = idx
      | otherwise = indexFinder el xs (idx + 1)

-- ** L 2.3

--

-- | Implement a recursive function for integer multiplication in terms of addition.
-- This implementation should work correctly for positive numbers and 0 (don't worry
-- about negative numbers)
lb23 :: Int -> Int -> Int
lb23 first second = multiplicationHelper (min first second) (max first second) 0
  where
    multiplicationHelper 0 _ acc = acc
    multiplicationHelper x y acc = multiplicationHelper (x - 1) y (acc + y)

-- ** L 2.4

--

-- | Implement a function which computes all the levels of a sum triangle for a list.
-- The level of a triangle sum has one fewer element than the previous level and
-- each element is computed as a sum of the two consecutive elements in the previous
-- level.
-- Example:
-- lb24 [1.0, 2.0, 3.0, 4.0] = [[1.0, 2.0, 3.0, 4.0], [3.0, 5.0, 7.0], [8.0, 12.0], [20.0]]
-- In the case of an empty list, return an empty list.
lb24 :: [Double] -> [[Double]]
lb24 [] = []
lb24 ls@(_ : xs) = ls : lb24 nextLevel
  where
    nextLevel = incrementListByPrevious ls 
    incrementListByPrevious [] = []
    incrementListByPrevious (x1 : x2 : xs) = (x1 + x2) : incrementListByPrevious (x2:xs)
    incrementListByPrevious (x) = [] 
