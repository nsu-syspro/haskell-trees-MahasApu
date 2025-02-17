{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (foldl, foldr)


-- foldr:: 

-- * Type definitions

-- | Binary tree
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving Show

-- | Forest (i.e. list of 'Tree's)
type Forest a = [Tree a]

-- | Tree traversal order
data Order = PreOrder | InOrder | PostOrder
  deriving Show
  

-- * Function definitions

-- | Returns values of given 'Tree' in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> torder PreOrder   (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- "A.B.."
-- >>> torder InOrder    (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- ".A.B."
-- >>> torder PostOrder  (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- "...BA"

--
torder :: Order    -- ^ Order of resulting traversal
       -> Maybe a  -- ^ Optional leaf value
       -> Tree a   -- ^ Tree to traverse
       -> [a]      -- ^ List of values in specified order
torder PreOrder  l t = preorder  l t
torder InOrder   l t = inorder   l t
torder PostOrder l t = postorder l t

-- | Pre-order traversal
--
preorder :: Maybe a -> Tree a -> [a]
preorder Nothing  Leaf         = [ ]
preorder (Just a) Leaf         = [a]
preorder leaf (Branch a l r)   = [a] ++ preorder leaf l ++ preorder leaf r 

-- | In-order traversal
--
inorder :: Maybe a -> Tree a -> [a]
inorder Nothing  Leaf         = [ ]
inorder (Just a) Leaf         = [a]
inorder leaf (Branch a l r)   = inorder leaf l ++ [a] ++ inorder leaf r

-- | Post-order traversal
--
postorder :: Maybe a -> Tree a -> [a]
postorder Nothing  Leaf         = [ ]
postorder (Just a) Leaf         = [a]
postorder leaf (Branch a l r)   = postorder leaf l ++ postorder leaf r ++ [a]


-- | Returns values of given 'Forest' separated by optional separator
-- where each 'Tree' is traversed in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> forder PreOrder  (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|C..|A.B.."

-- >>> forder InOrder   (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|.C.|.A.B."

-- >>> forder PostOrder (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]

--
forder :: Order     -- ^ Order of tree traversal
       -> Maybe a   -- ^ Optional separator between resulting tree orders
       -> Maybe a   -- ^ Optional leaf value
       -> Forest a  -- ^ List of trees to traverse
       -> [a]       -- ^ List of values in specified tree order
forder _ _ _ [ ]            = [ ]
forder order sep l (x : xs) = foldl (forderStep order sep l) (torder order l x) xs

-- | Helper function for `foldl`, handling tree traversal and separator insertion.
--
forderStep :: Order     -- ^ Order of tree traversal
           -> Maybe a   -- ^ Optional separator between resulting tree orders
           -> Maybe a   -- ^ Optional leaf value
           -> [a]       -- ^ Accumulator for folding
           -> Tree a    -- ^ Tree to traverse
           -> [a]       -- ^ List of values in specified tree order
forderStep order sep l acc tree = 
           acc ++ maybeToList sep ++ torder order l tree


-- | Converts a `Maybe` value to a list.
--
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = [ ]
maybeToList (Just a) = [a]

-- | Left fold, accumulates a result by applying an operation to 
-- each element from recieved list [b] from left to right
--
-- Usage example:
-- >>> foldl (+) 5 [1,2,3,4]
-- 15
-- >>> foldl (/) 2 []
-- 2.0
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ acc  [ ]      = acc
foldl op acc (x : xs) = foldl op (op acc x) xs


-- | Right fold, accumulates a result by applying an operation to 
-- each element from recieved list [b] from right to left
--
-- Usage example:
-- >>> foldr (+) 5 [1,2,3,4]
-- 15
-- >>> foldr max 5 [1,2,3,4]
-- 5
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc  [ ]      = acc
foldr op acc (x : xs) = op x (foldr op acc xs)

