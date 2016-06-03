{-|
Module     : KDTree
Description: A simple KD Tree.
-}

module KDTree (
    KDTree,
    construct_simple_KDTree,
    construct_KDTree,
    split,
    is_leaf,
    width,
    set_leaf_val,
    get_leaf_val,
    leaf_boundaries,
    children
) where

import Data.Foldable
import qualified Data.Sequence as S

-- | A KD Tree that stores data in its leaves
data KDTree a b
      -- | A node with multiple children
    = Tree [KDTree a b]

    -- | A Leaf node that stores its boundaries and some other value
    | Leaf (S.Seq (a, a)) b deriving Show

-- | Constructs a KDTree consisting of a single leaf with the given boundaries
-- and other value
construct_simple_KDTree :: [(a, a)] -> b -> KDTree a b
construct_simple_KDTree borders val = Leaf (S.fromList borders) val

-- | Construct a KDTree with 2 children
construct_KDTree :: KDTree a b -> KDTree a b -> KDTree a b
construct_KDTree left right = Tree [left, right]

-- | Splits a leaf into two leaves. If passed a non-leaf, throws an error
-- non-leaf
split :: (Num a, Ord a, Fractional a) => KDTree a b -> (KDTree a b, KDTree a b)
split t@(Tree _ ) = error "KDTree: tried to split a non-leaf node"
split (Leaf boundaries val) = (Leaf lower_part val, Leaf upper_part val)
    where
        upper_part = S.update max_dim_index (splitting_point, max) boundaries
        lower_part = S.update max_dim_index (min, splitting_point) boundaries
        splitting_point = (min + max) / 2
        (min, max) = S.index boundaries max_dim_index        
        (max_dim_index, _) = maximumBy (\(_, a) (_, b) -> compare a b) indexed_widths
        indexed_widths = zip [0 .. ] $
            map (\(low, high) -> high - low) $ toList boundaries

-- | Checks whether a given KDTree is a leaf node
is_leaf :: KDTree a b -> Bool
is_leaf (Tree _ )   = False
is_leaf (Leaf _ _ ) = True

-- | If passed a leaf node, returns the node's width. Otherwise, throws an error
width :: (Num a, Ord a) => KDTree a b -> a
width (Tree _ ) = error "KDTree: Tried to get width of non-leaf node"
width (Leaf boundaries _) = maximum $ map (\(low, high) -> high - low) $ toList boundaries

-- | If passed a leaf, sets the leaf's value. Otherwise, throws an error
set_leaf_val :: KDTree a b -> b -> KDTree a b
set_leaf_val t@(Tree _) _ = error "KDTree: tried to set value of non-leaf node"
set_leaf_val (Leaf boundaries _) val = Leaf boundaries val

-- | If passed a leaf, returns the leaf's value. Otherwise, throws an error
get_leaf_val :: KDTree a b -> b
get_leaf_val (Leaf _ val) = val
get_leaf_val (Tree children) = error "KDTree: tried to get value of non-leaf node"

-- | If passed a leaf, returns the a list of the leaf's boundaries. Otherwise, returns an
-- empty list.
leaf_boundaries :: KDTree a b -> [(a, a)]
leaf_boundaries (Tree _ ) = []
leaf_boundaries (Leaf boundaries _) = toList boundaries

-- | Returns a list of a node's children
children :: KDTree a b -> [KDTree a b]
children (Leaf _ _) = [] 
children (Tree tree_children) = tree_children
