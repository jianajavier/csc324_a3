{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}
--ADDED THIS MODULE LINE BECAUSE A3_TEST_MUTATIONUSER.HS WAS GIVING TROUBLE
module MutationUser (
    pointerTest
    )
    where

import Mutation (
    get, set, def, Mutable, Pointer(..), Memory,
    StateOp(..), returnVal, (>>>), (>~>), runOp
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
