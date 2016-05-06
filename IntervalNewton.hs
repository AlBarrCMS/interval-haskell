module IntervalNewton where


import ImageWriter
import Interval
import KDTree
import Polynomial
import PolynomialParser

p :: (Read a, Num a, Floating a) => Polynomial a
p = poly
    where
        Just poly = parse_polynomial "-1 + 3 x + 2 x^2 - 4.5 x^3 - 0.666 x^4 + 2.025 x^5 + 3 y^1+ 6 x^1 y^1-  6 x^2 y^1- 9 x^3 y^1+ 2 x^4 y^1+ 4.05 x^5 y^1+ 4.5 y^2 - 6 x^1 y^2 -  9 x^2 y^2 + 9 x^3 y^2 + 3 x^4 y^2 - 4.05 x^5 y^2 - 4.5 y^3 -  4 x^1 y^3 + 9 x^2 y^3 + 6 x^3 y^3 - 3 x^4 y^3 - 2.7 x^5 y^3 -  3.375 y^4 + 2 x^1 y^4 + 6.75 x^2 y^4 - 3 x^3 y^4 - 2.25 x^4 y^4 +  1.35 x^5 y^4 + 2.025 y^5 + 0.8 x^1 y^5 - 4.05 x^2 y^5 - 1.2 x^3 y^5 +  1.35 x^4 y^5 + 0.54 x^5 y^5"

inclusion :: (Eq a, Ord a, Num a) => Polynomial a -> [Char] -> [Interval a] -> Interval a
inclusion poly vars domains = to_const $ evaluate vars domains (fmap from_num ctf)
    where
        ctf = corner_form vars domains poly

corner_form :: (Eq a, Ord a, Num a) =>
    [Char] -> [Interval a] -> Polynomial a -> Polynomial a
corner_form vars intervals poly = taylor_expand vars (map corner intervals) poly
