{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}
module AList (
    AList,
    lookupA,
    insertA,
    updateA,
    keyExists,
    deleteA
    )
    where


type AList a b = [(a, b)]

-- | Returns the value in the association list corresponding to the given key.
--   Assumes that the key is in the association list.
lookupA :: Eq a => AList a b -> a -> b

-- Don't need a base case with empty list because it assumes the key is in association list
lookupA alist key =
    let (a, b) = head alist
    in
    if (a == key) then
        b
    else
        lookupA (tail alist) key

keyExists key [] = False
keyExists key alst =
    let (a, b) = head alst
    in
    if (a == key) then
        True
    else
        keyExists key (tail alst)

-- | Returns a new association list which is the old one, except with
--   the new key-value pair inserted. However, it returns the *same* list
--   if the key already exists in the list.
insertA :: Eq a => AList a b -> (a, b) -> AList a b
insertA alist (key, val) =
    if (keyExists key alist) then
        alist
    else
        alist ++ [(key, val)]


-- | Returns a new association list which is the old one, except with
--   the value corresponding to the given key changed to the given new value.
--   However, it returns the *same* list if the key doesn't appear in the list.
updateA :: Eq a => AList a b -> (a, b) -> AList a b
updateA alist (key, val) =
    map (\x -> let (a, b) = x in
        if (a == key) then (a, val) else (a, b)) alist

-- | Deletes a key value pair from the association list alist
--   with the given key. If it does not find the key, it returns
--   the same list.
deleteA :: Eq a => AList a b -> a -> AList a b
deleteA alist key =
  filter (\x -> let (k, v) = x in k /= key) alist
