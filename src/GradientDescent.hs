{-|
  Module       : GradientDescent
  Description  : Perform gradient descent to find local minima of a polynomial
 -}
module GradientDescent (
  gradient_descent,
  bounded_gradient_descent
) where
  import Polynomial
  import Interval
  import Debug.Trace

  gradient_descent :: (Show a, Num a, Ord a, Fractional a)
                   => a -> [Char] -> Int -> Polynomial a -> [a] -> a
  gradient_descent precision vars depth poly start
    | norm == 0 = old_value
    | norm > precision && depth > 0 = gradient_descent precision vars (depth - 1) poly new_point
    | otherwise = value
    where
      grad = map to_const $ map (evaluate vars start) $ gradient vars poly
      norm = sum $ zipWith (*) grad grad
      normalized_grad = map (/norm) grad
      new_point = zipWith (-) start normalized_grad
      value = to_const $ evaluate vars new_point poly
      old_value = to_const $ evaluate vars start poly

  bounded_gradient_descent :: (Show a, Num a, Ord a, Fractional a)
                   => a -> [Char] -> [Interval a] -> Int -> Polynomial a -> [a] -> a
  bounded_gradient_descent precision vars region depth poly start
    | norm == 0 || not in_boundary  = old_value
    | norm > precision && depth > 0 = bounded_gradient_descent precision vars region (depth - 1) poly new_point
    | otherwise = value
    where
      grad = map to_const $ map (evaluate vars start) $ gradient vars poly
      norm = sum $ zipWith (*) grad grad
      normalized_grad = map (/norm) grad
      new_point = zipWith (-) start normalized_grad
      value = to_const $ evaluate vars new_point poly
      old_value = to_const $ evaluate vars start poly
      in_boundary = and $ zipWith contains region new_point
