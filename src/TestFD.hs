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

import Debug.Trace

-- | Main method. Currently prints out the result of Interval Newton run on the
-- command line arguments to a ppm or csv file. The first argument is the polynomial as
-- a string and the second is the output file name. The file name must end in either
-- .ppm or .csv
main :: IO ()
main =
  do
    args <- getArgs
    p_string <- return $ args !! 0
    xmin <- return (read $ args !! 1 :: Float)
    xmax <- return (read $ args !! 2 :: Float)
    ymin <- return (read $ args !! 3 :: Float)
    ymax <- return (read $ args !! 4 :: Float)
    poly <- return $ fromJust $ parse_polynomial p_string
    p_leaf_data <- return
        $! leaf_rin_solve ((sum . map (\x -> x * x) . partials "xy") poly)
                          "xy"
                          0.0001
                          0.0001
                          0.0000001
                          (1 / 0, poly)
                          []
                          next_min
                          [Interval xmin xmax, Interval ymin ymax]
    putStrLn (rin_write_leaf_data p_leaf_data)
      where
        next_min :: (Num a, Ord a)
                 => (a, Polynomial a)
                 -> [Interval a]
                 -> (a, Polynomial a)
        next_min (current_min, poly) region =
            (min (local_min region poly) current_min, poly)

        local_min :: (Num a, Ord a) => [Interval a] -> Polynomial a -> a
        local_min [Interval amin amax,
                   Interval bmin bmax]
                  poly=
            minimum (map (\x -> to_const (evaluate "xy" x poly)) combos)
          where
            combos = [[x, y] | x <- [amin, amax], y <- [bmin, bmax]]
