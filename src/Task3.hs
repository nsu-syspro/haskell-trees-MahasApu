{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (compare, foldl, foldr, Ordering(..))

import Task1 (Tree(..))
import Task2(Cmp, compare, listToBST, bstToList, tinsert, tlookup, tdelete)

-- * Type definitions

-- | Tree-based map
type Map k v = Tree (k, v)

compareNode :: Ord k => Cmp (k, v)
compareNode (x, _) (y, _) = compare x y


-- * Function definitions

-- | Construction of 'Map' from association list
--
-- Usage example:
--
-- >>> listToMap [(2,'a'),(3,'c'),(1,'b')]
-- WAS Branch (2,'a') (Branch (1,'b') Leaf Leaf) (Branch (3,'c') Leaf Leaf)
-- NOW Branch (1,'b') Leaf (Branch (3,'c') (Branch (2,'a') Leaf Leaf) Leaf)
-- Well, it's also a correct BST
-- >>> listToMap [] :: Map Int Char
-- Leaf
--
listToMap :: Ord k => [(k, v)] -> Map k v
listToMap = listToBST compareNode 

-- | Conversion from 'Map' to association list sorted by key
--
-- Usage example:
--
-- >>> mapToList (Branch (2,'a') (Branch (1,'b') Leaf Leaf) (Branch (3,'c') Leaf Leaf))
-- [(1,'b'),(2,'a'),(3,'c')]
-- >>> mapToList Leaf
-- []
--
mapToList :: Map k v -> [(k, v)]
mapToList = bstToList

-- | Searches given 'Map' for a value associated with given key
--
-- Returns value associated with key wrapped into 'Just'
-- if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> mlookup 1 (Branch (2,'a') (Branch (1,'b') Leaf Leaf) (Branch (3,'c') Leaf Leaf))
-- WAS Just 'a'
-- NOW Just 'b'
-- >>> mlookup 'a' Leaf
-- Nothing
--
mlookup :: Ord k => k -> Map k v -> Maybe v
mlookup k tree 
    | Just (_, v) <- tlookup compareNode (k, undefined) tree = Just v
    | otherwise = Nothing


-- | Inserts given key and value into given 'Map'
--
-- If given key was already present in the 'Map'
-- then replaces its value with given value.
--
-- Usage example:
--
-- >>> minsert 0 'd' (Branch (2,'a') (Branch (1,'b') Leaf Leaf) (Branch (3,'c') Leaf Leaf))
-- Branch (2,'a') (Branch (1,'b') (Branch (0,'d') Leaf Leaf) Leaf) (Branch (3,'c') Leaf Leaf)
-- >>> minsert 1 'X' (Branch (2,'a') (Branch (1,'b') Leaf Leaf) (Branch (3,'c') Leaf Leaf))
-- WAS Branch (2,'a') (Branch (1,'X') Leaf Leaf) (Branch (3,'c' Leaf Leaf)
-- NOW Branch (2,'a') (Branch (1,'X') Leaf Leaf) (Branch (3,'c') Leaf Leaf)
-- There is a missing closing bracket in the tree
-- >>> minsert 1 'X' Leaf
-- Branch (1,'X') Leaf Leaf
--
minsert :: Ord k => k -> v -> Map k v -> Map k v
minsert k v = tinsert compareNode (k, v) 

-- | Deletes given key from given 'Map'
--
-- Returns updated 'Map' if the key was present in it;
-- or unchanged 'Map' otherwise.
--
-- Usage example:
--
-- >>> mdelete 1 (Branch (2,'a') (Branch (1,'b') Leaf Leaf) (Branch (3,'c') Leaf Leaf))
-- Branch (2,'a') Leaf (Branch (3,'c') Leaf Leaf)
-- >>> mdelete 'a' Leaf
-- Leaf
--
mdelete :: Ord k => k -> Map k v -> Map k v
mdelete k = tdelete compareNode (k, undefined)
