import ImageWriter
import Interval
import IntervalNewton
import KDTree
import Polynomial
import PolynomialParser

import System.Environment

p :: (Read a, Num a, Floating a) => Polynomial a
p = poly
    where
        {-Just poly = parse_polynomial "-1 + 3 x + 2 x^2 - 4.5 x^3 - 0.666 x^4 + 2.025 x^5 + 3 y^1+ 6 x^1 y^1-  6 x^2 y^1- 9 x^3 y^1+ 2 x^4 y^1+ 4.05 x^5 y^1+ 4.5 y^2 - 6 x^1 y^2 -  9 x^2 y^2 + 9 x^3 y^2 + 3 x^4 y^2 - 4.05 x^5 y^2 - 4.5 y^3 -  4 x^1 y^3 + 9 x^2 y^3 + 6 x^3 y^3 - 3 x^4 y^3 - 2.7 x^5 y^3 -  3.375 y^4 + 2 x^1 y^4 + 6.75 x^2 y^4 - 3 x^3 y^4 - 2.25 x^4 y^4 +  1.35 x^5 y^4 + 2.025 y^5 + 0.8 x^1 y^5 - 4.05 x^2 y^5 - 1.2 x^3 y^5 +  1.35 x^4 y^5 + 0.54 x^5 y^5"-}
        Just poly = parse_polynomial "-1 + xy + x^2 + y^2 + x^5y^3"

unsafe_parse :: (Read a, Num a, Floating a) => String -> Polynomial a
unsafe_parse poly_string = poly
    where
        Just poly = parse_polynomial poly_string

main :: IO ()
main = do
    args <- getArgs
    p_string <- return $ args !! 0
    filename <- return $ args !! 1
    poly <- return $ unsafe_parse p_string  
    {-putStrLn $ "Polynomial parsed as: " ++ (show poly) -}
    p_zeroes <- return $! zeros poly "xy" [Interval (-3) 3, Interval (-3) 3] 0.05
    image <- return $ write_to_image p_zeroes 6.0 6.0 3.0 3.0 1000 1000
    writeFile filename (write_to_ppm 255 image)
