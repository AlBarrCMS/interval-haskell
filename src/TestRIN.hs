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
    filename <- return $ args !! 1
    poly <- return $ fromJust $ parse_polynomial p_string
    putStrLn $ "Polynomial parsed as: " ++ (show poly)
    p_zeroes <- return
        $! rin_solve poly
                     "xy"
                     0.0001
                     0.0001
                     0.0001
                     [Interval (-3) 3, Interval (-3) 3]
    p_leaf_data <- return
        $! leaf_rin_solve poly
                          "xy"
                          0.0001
                          0.0001
                          0.0000001
                          [Interval (-3) 3, Interval (-3) 3]
    image <- return $ write_to_image p_zeroes 6.0 6.0 3.0 3.0 500 500
    print $ length p_zeroes
    if isSuffixOf ".csv" filename
      then
        writeFile filename $ rin_write_leaf_data p_leaf_data
      else if isSuffixOf ".ppm" filename then
        writeFile filename (write_to_ppm 255 image)
      else
        putStrLn "Error: invalid filename"
