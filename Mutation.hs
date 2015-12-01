{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer(..), Value(..)
    )
    where

import AList (AList, lookupA, insertA, updateA, keyExists)

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer deriving Show

-- Type class representing a type which can be stored in "Memory".
class Mutable a where

    -- Look up a value in memory referred to by a pointer.
    get :: Memory -> Pointer a -> a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Memory -> Pointer a -> a -> Memory

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Memory -> Integer -> a -> (Pointer a, Memory)

instance Mutable Integer where
    get mem (P val) = 
        if keyExists val mem then
            case lookupA mem val of 
                IntVal x -> x
        else
            error "Key does not exist in memory"
    set mem (P pt) val = updateA mem (pt, IntVal val)
    def mem i val = 
        if keyExists i mem then
            error "Key already exists in memory"
        else
            ((P i), insertA mem (i, IntVal val)) 

instance Mutable Bool where
    get mem (P val) = case lookupA mem val of 
        BoolVal x -> x
    set mem (P pt) val = updateA mem (pt, BoolVal val)
    def mem i val = ((P i), insertA mem (i, BoolVal val))

testMem :: Memory
testMem = [(1, IntVal 10), (2, IntVal 30), (3, BoolVal True), (4, BoolVal False)]

p3 :: Pointer Bool 
p3 = P 3

p1 :: Pointer Integer
p1 = P 1

-- Maybe might need to add more error checking?
-- Did not test against actual test file