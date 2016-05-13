import ImageWriter
import Interval
import IntervalNewton
import KDTree
import Polynomial
import PolynomialParser

main :: IO ()
main = do
    p_zeroes <- return $! zeros p "xy" [Interval (-3) 3, Interval (-3) 3] 0.01
    {-print $ p_zeroes -}
    image <- return $ write_to_image p_zeroes 6.0 6.0 3.0 3.0 1000 1000
    {-putStrLn $ write_to_ppm 255 $ image-}
    {-print $ p_zeroes -}
    appendFile "test.ppm" (write_to_ppm 255 image)
