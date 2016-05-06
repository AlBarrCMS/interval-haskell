module KDTree where

import Data.Foldable
import qualified Data.Sequence as S

data KDTree a = Tree (KDTree a) (KDTree a) | Leaf (S.Seq (a, a)) deriving Show

construct_simple_KDTree :: (Num a) => [(a, a)] -> KDTree a
construct_simple_KDTree borders = Leaf $ S.fromList borders

-- splits a leaf into two leaves. Doesn't affect non-leaves
split :: (Num a, Ord a, Fractional a) => KDTree a -> KDTree a
split t@(Tree _ _) = t
split (Leaf boundaries) = Tree (Leaf lower_part) (Leaf upper_part)
    where
        upper_part = S.update max_dim_index (splitting_point, max) boundaries
        lower_part = S.update max_dim_index (min, splitting_point) boundaries
        splitting_point = (min + max) / 2
        (min, max) = S.index boundaries max_dim_index        
        (max_dim_index, _) = maximumBy (\(_, a) (_, b) -> compare a b) indexed_widths
        indexed_widths = zip [0 .. ] $
            map (\(low, high) -> high - low) $ toList boundaries
