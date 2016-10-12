import ImageWriter
import Interval
import IntervalNewton
import KDTree
import Polynomial
import PolynomialParser

import Data.List
import Data.Maybe
import System.Environment

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
    p_zeroes <- return $! zeros poly "xy" [Interval (-3) 3, Interval (-3) 3] 0.001
    image <- return $ write_to_image p_zeroes 6.0 6.0 3.0 3.0 500 500
    tree_string <- return $! leaves poly "xy" [Interval (-3) 3, Interval (-3) 3] 0.001
    if isSuffixOf ".csv" filename
      then
        writeFile filename $ write_leaf_data tree_string
      else
        if isSuffixOf ".ppm" filename
          then
            writeFile filename (write_to_ppm 255 image)
          else
            putStrLn "Error: invalid filename"
