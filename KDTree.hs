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

data KDTree a b = Tree (KDTree a b) (KDTree a b) | Leaf (S.Seq (a, a)) b deriving Show

construct_simple_KDTree :: [(a, a)] -> b -> KDTree a b
construct_simple_KDTree borders val = Leaf (S.fromList borders) val

construct_KDTree :: KDTree a b -> KDTree a b -> KDTree a b
construct_KDTree left right = Tree left right

-- splits a leaf into two leaves. Doesn't affect non-leaves
split :: (Num a, Ord a, Fractional a) => KDTree a b -> (KDTree a b, KDTree a b)
split t@(Tree _ _) = (t, t)
split (Leaf boundaries val) = (Leaf lower_part val, Leaf upper_part val)
    where
        upper_part = S.update max_dim_index (splitting_point, max) boundaries
        lower_part = S.update max_dim_index (min, splitting_point) boundaries
        splitting_point = (min + max) / 2
        (min, max) = S.index boundaries max_dim_index        
        (max_dim_index, _) = maximumBy (\(_, a) (_, b) -> compare a b) indexed_widths
        indexed_widths = zip [0 .. ] $
            map (\(low, high) -> high - low) $ toList boundaries

is_leaf :: KDTree a b -> Bool
is_leaf (Tree _ _ ) = False
is_leaf (Leaf _ _ )   = True

width :: (Num a, Ord a) => KDTree a b -> a
width (Tree _ _) = -1
width (Leaf boundaries _) = maximum $ map (\(low, high) -> high - low) $ toList boundaries

set_leaf_val :: KDTree a b -> b -> KDTree a b
set_leaf_val t@(Tree _ _) _ = t
set_leaf_val (Leaf boundaries _) val = Leaf boundaries val

get_leaf_val :: KDTree a b -> b
get_leaf_val (Leaf _ val) = val
get_leaf_val (Tree left _) = get_leaf_val left

leaf_boundaries :: KDTree a b -> [(a, a)]
leaf_boundaries (Tree _ _) = []
leaf_boundaries (Leaf boundaries _) = toList boundaries

children :: KDTree a b -> (KDTree a b, KDTree a b)
children l@(Leaf _ _) = (l, l)
children (Tree left right) = (left, right)
