import ImageWriter
import Interval
import IntervalNewton
import KDTree
import Polynomial
import PolynomialParser
import RemainderIntervalNewton

import Data.List
import Data.Maybe
import System.Environment
import System.IO

import Debug.Trace
naive_necessary_boxes :: (RealFrac a, Floating a, Show a, Num a, Ord a)
  => Polynomial a -> a -> Int
naive_necessary_boxes poly resolution = trace (show resolution) $ length $ 
    zeros poly "xy" [Interval (-3) 3, Interval (-3) 3] resolution

rin_necessary_boxes :: (RealFrac a, Floating a, Show a, Num a, Ord a)
  => Polynomial a -> a -> Int
rin_necessary_boxes poly resolution = trace (show resolution) $ length $
    rin_solve poly "xy" resolution resolution resolution [Interval (-3) 3, Interval (-3) 3] 
rin_end_condition poly resolution = count_termination_rin_solve poly "xy" resolution resolution resolution [Interval (-3) 3, Interval (-3) 3]

-- | Main method. Currently prints out the result of Interval Newton run on the
-- command line arguments to a ppm or csv file. The first argument is the polynomial as 
-- a string and the second is the output file name. The file name must end in either
-- .ppm or .csv
main :: IO ()
main = do
  args <- getArgs
  p_string <- return $ args !! 0
  poly <- return $ fromJust $ parse_polynomial p_string
  resolutions <- return $ map (\x -> x / 10000000) $ map fromIntegral [10000, 9800 .. 10]
  rin_num_boxes <- trace (show resolutions) $ return $ map (rin_necessary_boxes poly) resolutions
  naive_num_boxes <- return $ map (naive_necessary_boxes poly) resolutions
  rin_terminations <- return $ rin_end_condition poly 0.0001
  rin_boxes <- return $ rin_solve poly "xy" 0.0001 0.0001 0.0001 [Interval (-3) 3, Interval (-3) 3]
  
  {-print $ [length rin_terminations, length rin_boxes]-}

  {-print $ length $ filter (== 1) rin_terminations-}
  {-print $ length $ filter (== 0) rin_terminations-}
  {-print $ length $ filter (== (-1)) rin_terminations-}

  print resolutions
  print rin_num_boxes
  print naive_num_boxes
