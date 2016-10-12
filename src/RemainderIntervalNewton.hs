module RemainderIntervalNewton (
  rin_solve,
  leaf_rin_solve,
  count_termination_rin_solve,
  rin_write_leaf_data
) where

import Data.List
import Data.Ord

import Debug.Trace

import Interval
import Polynomial

-- TODO: Remove Show typeclasses when they are unnecessary

-- The dot product of two vectors
dot :: (Num a) => [a] -> [a] -> a
dot = (sum .) . zipWith (*)

-- An infix operator for the dot product
(<.>) :: (Num a) => [a] -> [a] -> a
v <.> w = dot v w

vector_minus :: (Num a) => [a] -> [a] -> [a]
vector_minus = zipWith (-)

construct_monomial :: (Num a) => Char -> a -> Polynomial a
construct_monomial var constant = construct_univariate_polynomial var [-constant, 1]

-- Computes the linear Taylor Model approximation of a polynomial.
-- Using the first order taylor expansion of a polynomial with the Lagrange remainder,
-- you can find an inclusion function for the polynomial of the form
--   (x-c) . gradient + remainder interval
-- where the remainder interval depends on the hessian matrix of the polynomial and
-- the value of the polynomial at the point you are linearizing around.
-- See page 94 of the thesis for details. 
linearize_equation :: (Num a, Ord a)
  => [Char]            -- Variables in the polynomial
  -> [a]               -- Point to linearize around
  -> [Interval a]      -- Region to linearize over
  -> Polynomial a      -- Polynomial to linearize
  -> ([a], Interval a) -- (Gradient, Remainder interval) 
linearize_equation vars center bounds poly =
    (map (to_const . evaluate vars center) (gradient vars poly), remainder)
  where
    -- The result of conjugating a matrix (given in row-major order) by a vector 
    conjugate :: (Num a) => [a] -> [[a]] -> a
    conjugate vec mat = vec <.>(map (vec <.>) mat)

    -- The hessian matrix of the polynomial conjugated by the vector (x-c) where
    -- x is the vector of variables and c is the center
    hessian_product = to_const $ evaluate vars bounds $ (fmap from_num) $
        conjugate monomials $ hessian vars poly 
  
    remainder = hessian_product + (interval_evaluate vars center poly)

    -- Return the interval result of evaluating a (non interval) polynomial
    -- at a point
    interval_evaluate :: (Num a, Ord a) => [Char] -> [a] -> Polynomial a -> Interval a 
    interval_evaluate vars center poly = from_num $ to_const $ evaluate vars center poly
   
    -- Polynomials of the form (x-c) for the corresponding variables (x's) and center
    -- coordinates (c's) 
    monomials = zipWith construct_monomial vars center
    

-- Shrinks a region to be as small as possible while still containing the
-- interval linearization (inclusion function) of the polynomial
-- (See thesis p 96)
crop_equation :: (Num a, Ord a, Fractional a)
  => [Char]       -- Variables in the polynomial
  -> [a]          -- Point to linearize around
  -> [a]          -- Gradient of the polynomial (see linearize_equation)
  -> Interval a   -- Remaiinder interval of the polynomial (see linearize equation)
  -> [Interval a] -- Initial region of interest
  -> [Interval a] -- Cropped region of interest
crop_equation vars center grad remainder region = zipWith unsafe_intersection region bound
  where
    -- Monomials of the form (x-c) for the corresponding variables (x's) and center
    -- coordinates (c's) evaluated on the initial region. This is just found
    -- by subtracting the centers from the region components.
    evaluated_monomials = region `vector_minus` (map from_num center)

    -- Range of the inclusion function evaluated on the region
    inclusion = remainder + sum (zipWith scalar_mult grad evaluated_monomials)

    -- A list where the ith component is dp/dx_i (c_i) * (/x/_i - c_i)
    -- x_i is the ith variable, /x/_i, is the ith interval of the region, and c_i
    -- is the ith component of the center.
    partial_products = zipWith scalar_mult grad evaluated_monomials

    -- The component-wise inverse interval sum between the partial_products list and
    -- the inclusion function of the polynomial on the region
    numerators = zipWith (<->) (repeat inclusion) partial_products

    -- The component-wise division of the numerators by the gradient of the polynomial
    inclusion_term = zipWith scalar_div grad numerators 

    -- The component-wise interval difference between the center and terms computed above
    bound = (map from_num center) `vector_minus` inclusion_term


-- Divides a region in half along its longest edge.
simple_subdivide :: (Num a, Ord a, Fractional a, Show a)
  => [Interval a]                   -- Input region 
  -> [[Interval a]]                 -- [Lower sub-region, Upper sub-region]
simple_subdivide region = [lower_region, higher_region]
  where
    side_lengths = map width region

    -- The index of the longest side of the region
    max_index = snd $ maximumBy (comparing fst) (zip side_lengths [0..]) 

    -- The interval representing the longest side
    split_interval@(Interval low high) = region !! max_index
    lower_interval  = Interval low (midpoint split_interval)
    higher_interval = Interval (midpoint split_interval) high
    lower_region    = replaceAtIndex max_index region lower_interval
    higher_region   = replaceAtIndex max_index region higher_interval

    -- Replaces the nth element of list ls with item
    replaceAtIndex n ls item = a ++ (item:b)
      where (a, (_:b)) = splitAt n ls


-- Divides up a region along the edge that is the closest to being
-- perpindicular to the gradient of a function (these divisions best capture the
-- variation of the function). This process and the formula for the number of
-- pieces to split the region into are explained in the thesis around page 103.
rin_subdivide :: (Num a, Ord a, RealFrac a, Show a) 
  => [a]            -- Gradient of the polynomial whose zeros we want
  -> Interval a     -- Remainder interval
  -> [Interval a]   -- Region to be subdivided
  -> [[Interval a]] -- List of sub-regions
rin_subdivide grad remainder region = subregions
  where 
    widths = map width region
    gradient_weighted_widths = zipWith (*) (map width region) (map Prelude.abs grad)

    -- The index of the longest side of the region
    max_index = snd $ maximumBy (comparing fst) (zip gradient_weighted_widths [0..]) 

    subdivision_size = (width (region !! max_index)) / (width remainder) * (grad !! max_index)

    num_subdivisions = floor $ max 2 subdivision_size

    -- The interval representing the longest side
    (Interval low high) = region !! max_index

    -- The width of the side that we want to split into pieces
    split_side_width = high - low

    -- The width of each piece we want to split the side into
    subregion_width = split_side_width / (fromIntegral num_subdivisions)

    -- The points along the side where we will split it
    split_points = take (num_subdivisions + 1) $ scanl1 (+) (repeat subregion_width)

    -- The intervals that we are splitting the side into
    subregion_intervals = zipWith (Interval) split_points (tail split_points) 

    subregions = map (replaceAtIndex max_index region) subregion_intervals 

    -- Replaces the nth element of list ls with item
    replaceAtIndex n ls item = a ++ (item:b)
      where (a, (_:b)) = splitAt n ls

-- If the ratio between the longest and shortest sides of the region is too extreme
-- (or the gradient is 0), then just divide along the longest side. Otherwise, divide
-- along the side closed to perpindicular to the gradient (using rin_subdivide).
rin_limited_subdivide :: (Num a, Ord a, RealFrac a, Show a) 
  => [a]            -- Gradient of the polynomial whose zeros we want
  -> Interval a     -- Remainder interval
  -> a              -- Maximum sidelength ratio
  -> [Interval a]   -- Region to be subdivided
  -> [[Interval a]] -- List of sub-regions
rin_limited_subdivide grad remainder max_ratio region
  | zero_grad || (maximum widths) / (minimum widths) > max_ratio = simple_subdivide region
  | otherwise = rin_subdivide grad remainder region
  where
    widths = map width region
    zero_grad = and $ map ( == 0) grad


generic_rin_solve ::(Num a, Ord a, RealFrac a, Floating a, Show a)
  => ([Interval a] -> Interval a -> Int -> [b]) -- Function to bundle up returned data
  -> Polynomial a                             -- Polynomial whose zeros we want to find
  -> [Char]                                   -- Variables in polynomial
  -> a                                        -- The maximum size of a solution region
  -> a                                        -- The maximum widht of a linearized solution
  -> a                                        -- The minimum ratio of shortest to longest
                                              -- solution region size
  -> [Interval a]                             -- The region to search in
  -> [b]                                      -- The returned data
generic_rin_solve bundle_data poly vars max_soln_size max_lin_size max_ratio region
  | not contains_zero = bundle_data region inclusion_function (-1)
  | maximum (map width region) < max_soln_size = bundle_data region inclusion_function 0
  | (width remainder) / (norm gradient) < max_lin_size = bundle_data region inclusion_function 1
  | otherwise = concat subregion_solutions
    where
      center = map corner region
      (gradient, remainder) = linearize_equation vars center region poly
      monomials = region `vector_minus` (map from_num center)
      inclusion_function = (monomials <.> (map from_num gradient)) + remainder
      contains_zero = contains inclusion_function 0
      subregions = rin_limited_subdivide gradient remainder max_ratio region
      subregion_solutions = map (generic_rin_solve bundle_data poly vars max_soln_size max_lin_size max_ratio) subregions
      norm vec = sqrt $ sum $ map (\x -> x*x) vec

count_termination_rin_solve = generic_rin_solve (\_ _ end_case -> [end_case])

-- | Returns a list of regions that could contain zeros of a polynomial. These regions
-- are computed using the remainder interval newton method.
rin_solve :: (Num a, Ord a, RealFrac a, Floating a, Show a)
  => Polynomial a   -- The polynomial whose zeros we want
  -> [Char]         -- The variables in the polynomial
  -> a              -- The maximum size of a solution region
  -> a              -- The maximum width of a linearized solution
  -> a              -- The minimum ration of shortest to longest solution region size
  -> [Interval a]   -- The region to search in
  -> [[Interval a]] -- The acceptable solution regions
rin_solve = generic_rin_solve (\region inclusion _ -> if inclusion `contains` 0 then [region] else [])


-- | Returns the result of running remainder interval newton. Returns a list of
-- tuples. The first component is a region, and the second is the value of the inclusion
-- function on that region (so you can tell if the region contains 0 or not)
leaf_rin_solve :: (Num a, Ord a, RealFrac a, Floating a, Show a)
  => Polynomial a                 -- The polynomial whose zeros we want
  -> [Char]                       -- The variables in the polynomial
  -> a                            -- The maximum size of a solution region
  -> a                            -- The maximum width of a linearized solution
  -> a                            -- The minimum ration of shortest to longest
                                  -- solution region size
  -> [Interval a]                 -- The region to search in
  -> [([Interval a], Interval a)] -- The solution regions and rejected regions
leaf_rin_solve = generic_rin_solve (\region inclusion _ -> [(region, inclusion)])


-- | Returns a string containing the information from the leaves.
-- Leaves are separated by newlines. Each line contains the low value of the leaf's
-- interval, the high value of the leaf's interval, and the low and high values of the 
-- boundaries in each dimension. All values are separated by commas.
rin_write_leaf_data :: (Num a, Fractional a, RealFrac a, Show a) =>
    [([Interval a], Interval a)] -> IO()
{-rin_write_leaf_data leaves = concat $ map write_leaf leaves-}
rin_write_leaf_data leaves = mapM_ print $ map write_leaf leaves
    where
      write_leaf (region, (Interval low high)) =
        (intercalate ", " (map show (low : high : boundaries))) ++ "\n"
        where
          -- List of lower an upper bounds of leaf regions
          boundaries = concat $ map (\(Interval a b) -> [a, b]) region
