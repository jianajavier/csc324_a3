{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module CompoundMutation (
    Mutable, get, set, def,
    Memory, Pointer(..), Value(..), StateOp(..),
    runOp, (>>>), (>~>), returnVal,
    alloc, free
    )
    where

import AList (AList, lookupA, insertA, updateA, keyExists, deleteA)

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a person with two attributes:
-- age and whether they are a student or not.
data Person = Person Integer Bool deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer | PP Integer Integer deriving Show

-- Part 2: Chaining
data StateOp a = StateOp (Memory -> (a, Memory))

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

-- Then
(>>>) :: StateOp a -> StateOp b -> StateOp b
op1 >>> op2 = StateOp (\m ->
  let (_, m1) = runOp op1 m
  in runOp op2 m1)

-- Bind
(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
f >~> g = StateOp (\m ->
  let (x, m1) = runOp f m
      newStackOp = g x
  in runOp newStackOp m1)

(@@) :: Pointer a -> Integer -> Pointer b
personPointer @@ attr =
  let (PP p1 p2) = personPointer in
  if (attr == 1) then
    (P p1)
  else if (attr == 2) then
    (P p2)
    else
      error "No corresponding attribute"


age :: Integer
age = 1

isStudent :: Integer
isStudent = 2

{-
	Takes a value, then creates a new StateOp which doesn't interact
    with the memory at all, and instead just returns the value as the
    first element in the tuple.

    Example usage:

    g :: Integer -> StateOp Integer
    g x =
	    def 1 (x + 4) >~> \p ->
	    get p >~> \y ->
	    returnVal (x * y)

	> runOp (g 10) []
	(140, [(1, IntVal 14)])
-}
returnVal :: a -> StateOp a
returnVal x = StateOp (\m -> (x, m))

alloc :: Mutable a => a -> StateOp (Pointer a)
alloc x = StateOp (\m ->
  runOp (def (findFreeSpace 0 m) x) m)

findFreeSpace acc mem =
  if keyExists acc mem then
    findFreeSpace (acc + 1) mem
  else
    acc

free :: Mutable a => Pointer a -> StateOp ()
free (P val) = StateOp (\m ->
  if keyExists val m then
    ((), deleteA m val)
  else
    error "Key does not exist in memory")



-- Type class representing a type which can be stored in "Memory".
class Mutable a where

    -- Look up a value in memory referred to by a pointer.
    get :: Pointer a -> StateOp a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Pointer a -> a -> StateOp ()

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Integer -> a -> StateOp (Pointer a)

instance Mutable Integer where
  get (P val) = StateOp (\m -> (
    (if keyExists val m then
      case lookupA m val of
        IntVal x -> x
     else
      error "Key does not exist in memory"), m))

  set (P pt) val = StateOp (\m -> ((),
    if keyExists pt m then
      updateA m (pt, IntVal val)
    else
      error "Key does not exist in memory"))

  def i val = StateOp (\m ->
      if keyExists i m then
          error "Key already exists in memory"
      else
          ((P i), insertA m (i, IntVal val)))

instance Mutable Bool where
  get (P val) = StateOp (\m -> (
    (if keyExists val m then
      case lookupA m val of
        BoolVal x -> x
     else
      error "Key does not exist in memory"), m))

  set (P pt) val = StateOp (\m -> ((),
    if keyExists pt m then
      updateA m (pt, BoolVal val)
    else
      error "Key does not exist in memory"))

  def i val = StateOp (\m ->
    if keyExists i m then
      error "Key already exists in memory"
    else
      ((P i), insertA m (i, BoolVal val)))

instance Mutable Person where
  get (PP ip bp) = StateOp (\m ->
    let p1 = (P ip) :: Pointer Integer
        p2 = (P bp) :: Pointer Bool
        (age, m1) = runOp (get (p1 :: Pointer Integer)) m
        (bool, m2) = runOp (get (p2 :: Pointer Bool)) m
    in ((Person age bool), m))

  set (PP ip bp) person =
    let (Person age bool) = person
    in set (P ip) age >>> set (P bp) bool

  def i (Person age stud) = StateOp (\m ->
    let ((P num), val) = runOp (def i age >>> alloc stud) m
    in (PP i num, val))


personTest :: Person -> Integer -> StateOp (Integer, Bool, Person)
personTest person x =
    -- not using alloc, but we could
    def 1 person >~> \personPointer ->
    get (personPointer @@ age) >~> \oldAge ->
    set (personPointer @@ age) x >>>
    get (personPointer @@ isStudent) >~> \stu ->
    get (personPointer @@ age) >~> \newAge ->
    set personPointer (Person (2 * newAge) (not stu)) >>>
    get personPointer >~> \newPerson ->
    get (personPointer @@ isStudent) >~> \newStu ->
    returnVal (oldAge, newStu, newPerson)
