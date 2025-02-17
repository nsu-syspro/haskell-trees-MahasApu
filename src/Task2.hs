{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (compare, foldr, Ordering(..))

import Task1 (Tree(..),  inorder)


-- * Type definitions

-- | Ordering enumeration
data Ordering = LT | EQ | GT
  deriving (Show, Eq)

-- | Binary comparison function indicating whether first argument is less, equal or
-- greater than the second one (returning 'LT', 'EQ' or 'GT' respectively)
type Cmp a = a -> a -> Ordering

-- * Function definitions

-- | Binary comparison function induced from `Ord` constraint
--
-- Usage example:
--
-- >>> compare 2 3
-- LT
-- >>> compare 'a' 'a'
-- EQ
-- >>> compare "Haskell" "C++"
-- GT
--
compare :: Ord a => Cmp a
compare x y 
  | x < y     = LT
  | x > y     = GT
  | otherwise = EQ

-- | Conversion of list to binary search tree
-- using given comparison function
--
-- Usage example:
--
-- >>> listToBST compare [2,3,1]
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> listToBST compare ""
-- Leaf
--
listToBST :: Cmp a -> [a] -> Tree a
listToBST _ [] = Leaf
listToBST cmp (x:xs) = tinsert cmp x (listToBST cmp xs)

-- | Conversion from binary search tree to list
--
-- Resulting list will be sorted
-- if given tree is valid BST with respect
-- to some 'Cmp' comparison.
--
-- Usage example:
--
-- >>> bstToList (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- [1,2,3]
-- >>> bstToList Leaf
-- []
--
bstToList :: Tree a -> [a]
bstToList = inorder Nothing

-- | Tests whether given tree is a valid binary search tree
-- with respect to given comparison function
--
-- Usage example:
--
-- >>> isBST compare (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- True
-- >>> isBST compare (Leaf :: Tree Char)
-- True
-- >>> isBST compare (Branch 5 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- False
--
isBST :: Cmp a -> Tree a -> Bool
isBST  _   Leaf = True
isBST cmp (Branch v l r) = 
      lt v l 
      && gt v r 
      && isBST cmp l 
      && isBST cmp r
  where 
    lt _ Leaf                = True
    lt root (Branch x l1 r1) = cmp x root /= GT
                            && cmp x root /= EQ 
                            && lt root l1 
                            && lt root r1

    gt _ Leaf                = True
    gt root (Branch x l2 r2) = cmp x root /= LT  
                            && cmp x root /= EQ  
                            &&  gt root l2 
                            && gt root r2


-- | Searches given binary search tree for
-- given value with respect to given comparison
--
-- Returns found value (might not be the one that was given)
-- wrapped into 'Just' if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> tlookup compare 2 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Just 2
-- >>> tlookup compare 'a' Leaf
-- Nothing
-- >>> tlookup (\x y -> compare (x `mod` 3) (y `mod` 3)) 5 (Branch 2 (Branch 0 Leaf Leaf) (Branch 2 Leaf Leaf))
-- Just 2
--
tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup _ _ Leaf = Nothing
tlookup cmp x (Branch v l r) 
    | cmp x v == LT   = tlookup cmp x l
    | cmp x v == GT   = tlookup cmp x r 
    | otherwise       = Just v

-- | Inserts given value into given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- If the same value with respect to comparison
-- was already present in the 'Tree' then replaces it with given value.
--
-- Usage example:
--
-- >>> tinsert compare 0 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 (Branch 0 Leaf Leaf) Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 'a' Leaf
-- Branch 'a' Leaf Leaf
--
tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert   _ x  Leaf = Branch x Leaf Leaf
tinsert cmp x (Branch v l r) 
  | cmp x v == LT = Branch v (tinsert cmp x l) r
  | cmp x v == GT = Branch v l (tinsert cmp x r) 
  | otherwise     = Branch x l r

-- | Deletes given value from given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- Returns updated 'Tree' if the value was present in it;
-- or unchanged 'Tree' otherwise.
--
-- Usage example:
--
-- >>> tdelete compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 Leaf (Branch 3 Leaf Leaf)
-- >>> tdelete compare 'a' Leaf
-- Leaf
--
tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete  _ _ Leaf = Leaf
tdelete  cmp x (Branch v l r) 
  | cmp x v == LT = Branch v (tdelete cmp x l) r
  | cmp x v == GT = Branch v l (tdelete cmp x r)
  | otherwise     = deleteNode cmp (Branch v l r)

deleteNode :: Cmp a -> Tree a -> Tree a
deleteNode _  Leaf             = Leaf
deleteNode _ (Branch _ Leaf r) = r
deleteNode _ (Branch _ l Leaf) = l
deleteNode _ (Branch _ l r)    = Branch minR l newR
            where (minR, newR) = getMin r

getMin :: Tree a -> (a, Tree a)
getMin  Leaf             = error "Tree is empty!"
getMin (Branch v Leaf r) = (v, r)
getMin (Branch v l r)    = (minL, Branch v newL r)
      where (minL, newL) = getMin l

