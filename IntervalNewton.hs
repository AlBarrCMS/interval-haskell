module IntervalNewton (
    inclusion,
    zeros,
    leaves,
    write_to_image,
    write_leaf_data
) where

import ImageWriter
import Interval
import KDTree
import Polynomial
import PolynomialParser

import Data.List
import qualified Data.Sequence as S
import Debug.Trace

inclusion :: (Eq a, Ord a, Num a) => Polynomial a -> [Char] -> [Interval a] -> Interval a
inclusion poly vars domains = to_const $ evaluate vars domains (fmap from_num ctf)
    where
        ctf = corner_form vars domains poly

corner_form :: (Eq a, Ord a, Num a) =>
    [Char] -> [Interval a] -> Polynomial a -> Polynomial a
corner_form vars intervals poly = taylor_expand vars (map corner intervals) poly

zeros :: (Num a, Ord a, Fractional a, Show a) =>
    Polynomial a -> [Char] -> [Interval a] -> a -> [[Interval a]]
zeros poly vars vals max_width = zero_boxes poly vars max_width simple_box
    where
        simple_box = construct_simple_KDTree (map to_tuple vals) False

zero_boxes :: (Num a, Ord a, Fractional a, Show a) =>
    Polynomial a -> [Char] -> a -> KDTree a Bool -> [[Interval a]]
zero_boxes poly vars max_width tree
    | (is_leaf tree) && not contains_zero = []
    | (is_leaf tree) && contains_zero && (KDTree.width tree < max_width) =
            [map from_tuple $ leaf_boundaries tree]
    | (is_leaf tree) && contains_zero = lower_boxes ++ upper_boxes
    | otherwise = []
        where
            upper_boxes = zero_boxes poly vars max_width upper_child
            lower_boxes = zero_boxes poly vars max_width lower_child
            (lower_child, upper_child) = split tree
            contains_zero = contains range_estimate 0
            range_estimate = inclusion poly vars (map from_tuple $ leaf_boundaries tree)

leaves :: (Num a, Ord a, Fractional a, Show a) =>
    Polynomial a -> [Char] -> [Interval a] -> a -> [KDTree a (Interval a)]
leaves poly vars vals max_width = leaves_helper poly vars max_width simple_box
    where
        simple_box = construct_simple_KDTree (map to_tuple vals) (Interval 0 0)

leaves_helper :: (Num a, Ord a, Fractional a, Show a) =>
    Polynomial a -> [Char] -> a -> KDTree a (Interval a) -> [KDTree a (Interval a)]
leaves_helper poly vars max_width tree
    | (is_leaf tree) && not contains_zero = [set_leaf_val tree range_estimate]
    | (is_leaf tree) && contains_zero && (KDTree.width tree < max_width) = [set_leaf_val tree range_estimate]
    | (is_leaf tree) && contains_zero = lower_boxes ++ upper_boxes
    | otherwise = []
        where
            upper_boxes = leaves_helper poly vars max_width upper_child
            lower_boxes = leaves_helper poly vars max_width lower_child
            (lower_child, upper_child) = split tree
            contains_zero = contains range_estimate 0
            range_estimate = inclusion poly vars (map from_tuple $ leaf_boundaries tree)

write_leaf_data :: (Num a, Fractional a, RealFrac a, Show a) =>
    [KDTree a (Interval a)] -> String
write_leaf_data leaves = concat $ map write_leaf leaves
    where
        write_leaf leaf = (intercalate " " (map show (low : high : boundaries))) ++ "\n"
            where
                (Interval low high) = get_leaf_val leaf
                boundaries = concat $ map to_list $ leaf_boundaries leaf
                to_list :: (a, a) -> [a]
                to_list (x, y) = [x, y]

write_to_image :: (Num a, Fractional a, RealFrac a, Num b, Show a) =>
    [[Interval a]] -> a -> a -> a -> a -> a -> a -> Image b
write_to_image boxes tree_width tree_height off_x off_y image_width image_height =
    Image (round image_width) (round image_height) written_pixels
    where
        written_pixels = foldl'
            (\lst (x, y) -> S.update (x + (round image_height - y) * (round image_width)) marked lst)
            blank_pixels pixels_to_write

        pixels_to_write = concat $ map (\(xs:ys:_) -> cross xs ys) bounds
        bounds = map (map scale) $ map (take 2) boxes         

        scale (Interval x y) = Interval ((x + off_x) * image_width / tree_width) ((y + off_y) * image_height / tree_height)

        blank_pixels = S.fromList $ take (round (image_width * image_height))
                        $ repeat blank
        
        cross (Interval low_x high_x) (Interval low_y high_y) = 
            [(x, y) | x <- [(round low_x) .. (round high_x)],
                      y <- [(round low_y) .. (round high_y)]] 

blank :: (Num f) => Color f
blank = [255, 255, 255]

marked :: (Num f) => Color f
marked = [255, 0, 0]
