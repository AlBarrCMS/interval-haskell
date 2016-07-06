module RemainderIntervalNewton

where

import Interval
import Polynomial


linearize_equation :: (Num a, Ord a) =>
    [Char] -> [a] -> [Interval a] -> Polynomial a -> ([a], Interval a)
linearize_equation vars center bounds poly =
    (map (to_const . evaluate vars center) (gradient vars poly), range)
  where
    -- The dot product of two vectors
    dot :: (Num a) => [a] -> [a] -> a
    dot = (sum .) . zipWith (*)

    -- The result of conjugating a matrix (given in row-major order) by a vector 
    conjugate :: (Num a) => [a] -> [[a]] -> a
    conjugate vec mat = dot vec (map (dot vec) mat)

    hessian_product = to_const $ evaluate vars bounds $ (fmap from_num) $
        conjugate monomials $ hessian vars poly 
    
    range = hessian_product + (from_num $ to_const $ evaluate vars center poly)
  
    -- Polynomials of the form (x-c) for the corresponding variables (x's) and center
    -- coordinates (c's) 
    monomials = zipWith construct_monomial vars center
    
    construct_monomial :: (Num a) => Char -> a -> Polynomial a
    construct_monomial var constant = construct_univariate_polynomial var [-constant, 1]
