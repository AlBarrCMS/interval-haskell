module IntervalNewton (
    p,
    inclusion,
    zeros,
    write_to_image 
)where

import ImageWriter
import Interval
import KDTree
import Polynomial
import PolynomialParser

import Data.List
import qualified Data.Sequence as S
import Debug.Trace

p :: (Read a, Num a, Floating a) => Polynomial a
p = poly
    where
        Just poly = parse_polynomial "-1 + 3 x + 2 x^2 - 4.5 x^3 - 0.666 x^4 + 2.025 x^5 + 3 y^1+ 6 x^1 y^1-  6 x^2 y^1- 9 x^3 y^1+ 2 x^4 y^1+ 4.05 x^5 y^1+ 4.5 y^2 - 6 x^1 y^2 -  9 x^2 y^2 + 9 x^3 y^2 + 3 x^4 y^2 - 4.05 x^5 y^2 - 4.5 y^3 -  4 x^1 y^3 + 9 x^2 y^3 + 6 x^3 y^3 - 3 x^4 y^3 - 2.7 x^5 y^3 -  3.375 y^4 + 2 x^1 y^4 + 6.75 x^2 y^4 - 3 x^3 y^4 - 2.25 x^4 y^4 +  1.35 x^5 y^4 + 2.025 y^5 + 0.8 x^1 y^5 - 4.05 x^2 y^5 - 1.2 x^3 y^5 +  1.35 x^4 y^5 + 0.54 x^5 y^5"

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
        {-scale (Interval x y) = trace (show x ++ " " ++ show off_x) $ Interval ((x + off_x) * image_width / tree_width) ((y + off_y) * image_height / tree_height)-}

        blank_pixels = S.fromList $ take (round (image_width * image_height))
                        $ repeat blank
        
        cross (Interval low_x high_x) (Interval low_y high_y) = 
            [(x, y) | x <- [(round low_x) .. (round high_x)],
                      y <- [(round low_y) .. (round high_y)]] 

blank :: (Num f) => Color f
blank = [255, 255, 255]

marked :: (Num f) => Color f
marked = [255, 0, 0]
