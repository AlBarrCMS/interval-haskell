import ImageWriter
import Interval
import IntervalNewton
import KDTree
import Polynomial
import PolynomialParser
import MooreSkelboe

import Data.List
import Data.Maybe
import System.Environment

import Debug.Trace

-- | Main method. Currently prints out the result of Moore Skelboe run on the
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
    {-print $ poly-}
    {-print [xmin, xmax, ymin, ymax]-}
    p_leaf_data <- return
        $! leaf_minimize poly
                          "xy"
                          0.01
                          100
                          [Interval xmin xmax, Interval ymin ymax]
    {-print $ length p_leaf_data-}
    putStrLn (moore_skelboe_write_leaf_data p_leaf_data)
    -- image <- return $ write_to_image p_zeroes 6.0 6.0 3.0 3.0 500 500
    -- print $ length p_zeroes
    -- if isSuffixOf ".csv" filename
      -- then
        -- writeFile filename $ rin_write_leaf_data p_leaf_data
      -- else if isSuffixOf ".ppm" filename then
        -- writeFile filename (write_to_ppm 255 image)
      -- else
        -- putStrLn "Error: invalid filename"
