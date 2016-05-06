module Interval (
    Interval(Interval),
    from_num,
    midpoint,
    corner,
    width,
    radius,
    Interval.abs,
    mig,
    mag,
    sign,
    hull,
    neg,
    plus,
    minus,
    times,
    scalar_mult,
    divided,
    pow
) where

data Interval a = Interval a a

instance (Show a) => Show (Interval a) where
    show (Interval lower upper) = "[" ++ (show lower) ++ ", " ++ (show upper) ++ "]"

instance (Num a, Ord a) => Num (Interval a) where
    negate = neg
    (+) = plus
    (*) = times
    fromInteger n = Interval (fromInteger n) (fromInteger n)
    abs = id 
    signum interval = Interval 1 1 

instance (Num a, Fractional a, Ord a) => Fractional (Interval a) where
    (/) = divided
    fromRational q = Interval (fromRational q) (fromRational q)

instance (Eq a) => Eq (Interval a) where
    (==) (Interval a b) (Interval c d) = (a == c) && (b == d)

instance (Ord a) => Ord (Interval a) where
    compare (Interval a b) (Interval c d) = if lower_comp == EQ then upper_comp else lower_comp
        where lower_comp = compare a c
              upper_comp = compare b d

instance Functor (Interval) where
    fmap f (Interval lower upper) = Interval (f lower) (f upper)

from_num :: a -> Interval a
from_num n = Interval n n

midpoint :: (Num a, Fractional a) => Interval a -> a
midpoint (Interval lower upper) = (lower + upper) / 2

corner :: (Num a, Ord a) => Interval a -> a
corner (Interval lower upper) = if lower > 0 then lower else
                                if upper < 0 then upper else
                                0

width :: (Num a) => Interval a -> a
width (Interval lower upper) = upper - lower

radius :: (Num a, Fractional a) => Interval a -> a
radius (Interval lower upper) = (upper - lower) / 2

abs :: (Num a, Ord a) => Interval a -> Interval a
abs (Interval lower upper) = if lower > 0 then (Interval lower upper) else
                             if upper < 0 then (Interval (-upper) (-lower)) else
                             (Interval 0 upper)

mig :: (Num a, Ord a) => Interval a -> a
mig interval = lower where (Interval lower upper) = Interval.abs interval

mag :: (Num a, Ord a) => Interval a -> a
mag interval = upper where (Interval lower upper) = Interval.abs interval

sign :: (Num a, Ord a) => Interval a -> a
sign (Interval lower upper) = if upper <= 0 then -1 else
                              if lower >= 0 then 1 else
                              0

hull :: (Num a, Ord a) => [a] -> Interval a
hull lst = Interval (minimum lst) (maximum lst)

neg :: (Num a) => Interval a -> Interval a
neg (Interval lower upper) = Interval (-upper) (-lower)

plus :: (Num a) => Interval a -> Interval a -> Interval a
plus (Interval lower_a upper_a) (Interval lower_b upper_b) =
    Interval (lower_a + lower_b) (upper_a + upper_b)

minus :: (Num a) => Interval a -> Interval a -> Interval a
minus (Interval lower_a upper_a) (Interval lower_b upper_b) =
    Interval (lower_a - upper_b) (upper_a - lower_b)

times :: (Num a, Ord a) => Interval a -> Interval a -> Interval a
times (Interval low_a up_a) (Interval low_b up_b) =
    hull [low_a * low_b, up_a * low_b, low_a * up_b, up_a * up_b]

scalar_mult :: (Num a, Ord a) => a -> Interval a -> Interval a
scalar_mult s (Interval lower upper) = if s > 0 then Interval (s * lower) (s * upper)
                                       else Interval (s * upper) (s * lower)

divided :: (Num a, Ord a, Fractional a) => Interval a -> Interval a -> Interval a
divided (Interval low_a up_a) (Interval low_b up_b) = 
    hull [low_a / low_b, up_a / low_b, low_a / up_b, up_a / up_b]

pow :: (Num a, Ord a, Fractional a) => Interval a -> Int -> Interval a
pow interval n = if n >= 0 then pos_pow interval n else neg_pow interval n

pos_pow :: (Num a) => Interval a -> Int -> Interval a
pos_pow (Interval lower upper) n = Interval (lower^n) (upper^n)

neg_pow :: (Num a, Ord a, Fractional a) => Interval a -> Int -> Interval a
neg_pow interval n = divided (Interval 1 1) (pos_pow interval (-n))
