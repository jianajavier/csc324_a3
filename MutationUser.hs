{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}
--ADDED THIS MODULE LINE BECAUSE A3_TEST_MUTATIONUSER.HS WAS GIVING TROUBLE
module MutationUser (
    pointerTest, swap, swapCycle
    )
    where

import Mutation (
    get, set, def, Mutable, Pointer(..), Memory,
    StateOp(..), returnVal, (>>>), (>~>), runOp,
    Value(..)
    )

-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.
pointerTest :: Integer -> StateOp (Pointer Integer, Pointer Bool)
--((Pointer Integer, Pointer Bool), Memory)
pointerTest n = StateOp (\mem ->
  let (key, val) = runOp (def 100 (n + 3) >~> \x -> def 500 (n > 0)) mem
  in ((P 100, P 500), val))

-- Didn't use returnVal is that wrong?

--Works when you define Pointer Integer but you can't do this because they might both be bools
swap :: Mutable a => Pointer a -> Pointer a -> StateOp ()
--instance Mutable Integer where
swap (P p1) (P p2) = StateOp (\m ->
    let (ans1, mem1) = runOp (get (P p1)) m
        (ans2, mem2) = runOp (get (P p2)) m
    in
      runOp ((set ((P p1) :: Pointer Integer) ans2) >~> \x -> set ((P p2) :: Pointer Integer) ans1) m)
--instance Mutable Bool where
-- swap (P p1) (P p2) = StateOp (\m ->
--   let (ans1, mem1) = runOp (get (P p1)) m
--       (ans2, mem2) = runOp (get (P p2)) m
--   in
--     runOp ((set ((P p1) :: Pointer Bool) ans2) >~> \x -> set ((P p2) :: Pointer Bool) ans1) m)


swapCycle :: Mutable a => [Pointer a] -> StateOp ()
swapCycle [] = StateOp (\m -> ((), m))
swapCycle (x:[]) = StateOp (\m -> ((), m))
swapCycle (x:y:[]) = swap x y
swapCycle (x:y:rest) = swap x y >>> swapCycle (y:rest)

-- Test data
testMem :: Memory
testMem = [(1, IntVal 10), (2, BoolVal True), (3, BoolVal True), (4, BoolVal False)]

p1 :: Pointer Integer
p1 = P 1

p2 :: Pointer Integer
p2 = P 2

p3 :: Pointer Integer
p3 = P 3

p4 :: Pointer Bool
p4 = P 4
