{-|
  Module       : Interval
  Description  : Intervals which can be manipulated algebraically
 -}
module Interval (
  Interval(Interval),
  to_tuple,
  from_tuple,
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
  inverse_addition,
  (<->),
  times,
  scalar_mult,
  scalar_div,
  divided,
  pow,
  contains,
  intersection,
  unsafe_intersection,
  jsonify
) where
  import Data.Maybe

  -- | An interval (range between numbers)
  data Interval a = Interval a a

  instance (Show a) => Show (Interval a)
    where
      show (Interval lower upper) =
          "[" ++ (show lower) ++ ", " ++ (show upper) ++ "]"

  instance (Num a, Ord a) => Num (Interval a)
    where
      negate = neg
      (+) = plus
      (*) = times
      fromInteger n = Interval (fromInteger n) (fromInteger n)
      abs = id
      signum interval = Interval 1 1

  instance (Num a, Fractional a, Ord a) => Fractional (Interval a)
    where
      (/) = divided
      fromRational q = Interval (fromRational q) (fromRational q)

  instance (Eq a) => Eq (Interval a)
    where
      (==) (Interval a b) (Interval c d) = (a == c) && (b == d)

  instance (Ord a) => Ord (Interval a)
    where
      compare (Interval a b) (Interval c d) =
          if lower_comp == EQ then
            upper_comp
          else
            lower_comp
        where
          lower_comp = compare a c
          upper_comp = compare b d

  instance Functor (Interval)
    where
      fmap f (Interval lower upper) = Interval (f lower) (f upper)

  -- | Convert a value into an interval that only includes itself
  from_num :: a -> Interval a
  from_num n = Interval n n

  -- | Convert an interval into a tuple containing (min, max)
  to_tuple :: Interval a -> (a, a)
  to_tuple (Interval low high) = (low, high)

  -- | Create a tuple (min, max) into the interval [min, max]
  from_tuple :: (a, a) -> Interval a
  from_tuple (low, high) = Interval low high

  -- | Compute the midpoint of an interval (max - min) / 2
  midpoint :: (Num a, Fractional a) => Interval a -> a
  midpoint (Interval lower upper) = (lower + upper) / 2

  -- | Computes the corner of an interval. If the interval includes 0, the
  -- corner is 0. Otherwise, it is the bound of the interval that is closest to
  -- 0.
  corner :: (Num a, Ord a) => Interval a -> a
  corner (Interval lower upper) =
      if lower > 0 then
        lower
      else if upper < 0 then
        upper
      else
        0

  -- | Computes the width of an interval (max - min)
  width :: (Num a) => Interval a -> a
  width (Interval lower upper) = upper - lower

  -- | Computes the radius of an interval (half the width)
  radius :: (Num a, Fractional a) => Interval a -> a
  radius (Interval lower upper) = (upper - lower) / 2

  -- | Computes the interval which has the absolute values of the bounds of the
  -- input interval as bounds
  abs :: (Num a, Ord a) => Interval a -> Interval a
  abs (Interval lower upper) =
      if lower > 0 then
        (Interval lower upper)
      else if upper < 0 then
        (Interval (-upper) (-lower))
      else
        (Interval 0 (max (-lower) upper))

  -- | Computes the mignitude of an interval (the minimum element of the
  -- absolute value of the interval)
  mig :: (Num a, Ord a) => Interval a -> a
  mig interval = lower where (Interval lower upper) = Interval.abs interval

  -- | Computes the magnitude of an interval (the maximum element of the
  -- absolute value of the interval)
  mag :: (Num a, Ord a) => Interval a -> a
  mag interval = upper where (Interval lower upper) = Interval.abs interval

  -- | Computes the sign of an interval (0 if the interval includes 0, -1 if
  -- the interval is all below 0, and 1 if the interval is all above 0)
  sign :: (Num a, Ord a) => Interval a -> a
  sign (Interval lower upper) =
      if upper <= 0 then
        -1
      else if lower >= 0 then
        1
      else
        0

  -- | Generates the interval hull of a list of inputs (an interval that
  -- contains them all)
  hull :: (Num a, Ord a) => [a] -> Interval a
  hull lst = Interval (minimum lst) (maximum lst)

  -- | Generates an interval whose elements are the negations of the elements
  -- of the input interval
  neg :: (Num a) => Interval a -> Interval a
  neg (Interval lower upper) = Interval (-upper) (-lower)

  -- | Generates an interval whose elements are the sums of elements of the
  -- input intervals
  plus :: (Num a) => Interval a -> Interval a -> Interval a
  plus (Interval lower_a upper_a) (Interval lower_b upper_b) =
      Interval (lower_a + lower_b) (upper_a + upper_b)

  -- | Generates an interval whose elements are the differences of elements in
  -- the input intervals (the elements from the second interval are subttracted
  -- from those from the first interval).
  minus :: (Num a) => Interval a -> Interval a -> Interval a
  minus (Interval lower_a upper_a) (Interval lower_b upper_b) =
      Interval (lower_a - upper_b) (upper_a - lower_b)

  -- | Inverts interval addition (note that this is different than interval
  -- subtraction). If a = b + c, then inverse_addition a b = c
  inverse_addition :: (Num a) => Interval a -> Interval a -> Interval a
  inverse_addition (Interval lower_a upper_a) (Interval lower_b upper_b) =
      Interval (lower_a - lower_b) (upper_a - upper_b)

  (<->) :: (Num a) => Interval a -> Interval a -> Interval a
  a <-> b = inverse_addition a b

  -- | Generates an interval whose elements are the products of elements in the
  -- input intervals
  times :: (Num a, Ord a) => Interval a -> Interval a -> Interval a
  times (Interval low_a up_a) (Interval low_b up_b) =
      hull [low_a * low_b, up_a * low_b, low_a * up_b, up_a * up_b]

  -- | Generates an interval whose elements are the elements of the input
  -- interval multiplied by some scalar
  scalar_mult :: (Num a, Ord a) => a -> Interval a -> Interval a
  scalar_mult s (Interval lower upper) =
      if s > 0 then
        Interval (s * lower) (s * upper)
      else
        Interval (s * upper) (s * lower)

  -- | Generates an interval whose elements are the elements of the input
  -- interval divided by some scalar
  scalar_div :: (Num a, Ord a, Fractional a) => a -> Interval a -> Interval a
  scalar_div s (Interval lower upper) =
      if s > 0 then
        Interval (lower / s) (upper / s)
      else
        Interval (upper / s) (lower / s)

  -- | Generates an interval whose elements are the quotients of elements in
  -- the input intervals (the elements of the first interval are in the
  -- numerator and the elements of the second are in the denominator)
  divided :: (Num a, Ord a, Fractional a)
          => Interval a
          -> Interval a
          -> Interval a
  divided (Interval low_a up_a) (Interval low_b up_b) =
      hull [low_a / low_b, up_a / low_b, low_a / up_b, up_a / up_b]

  -- | Generates the interval whose elements are the powers of elements in the
  -- input interval
  pow :: (Num a, Ord a, Fractional a) => Interval a -> Int -> Interval a
  pow interval n =
      if n >= 0 then
        pos_pow interval n
      else
        neg_pow interval n
    where
      -- Finds power interval assuming that the power is positive
      pos_pow :: (Num a) => Interval a -> Int -> Interval a
      pos_pow (Interval lower upper) n = Interval (lower^n) (upper^n)

      -- Finds power interval assuming that the power is negative
      neg_pow :: (Num a, Ord a, Fractional a) => Interval a -> Int -> Interval a
      neg_pow interval n = divided (Interval 1 1) (pos_pow interval (-n))

  -- | Returns whether or not an interval contains a given value
  contains :: (Num a, Ord a) => Interval a -> a -> Bool
  contains (Interval low high) val = (low <= val) && (val <= high)

  -- | Returns the intersection of two intervals
  intersection :: (Ord a) => Interval a -> Interval a -> Maybe(Interval a)
  intersection (Interval lower_a upper_a) (Interval lower_b upper_b)
      | upper_a < lower_b = Nothing
      | upper_b < lower_a = Nothing
      | otherwise = Just $ Interval (max lower_a lower_b) (min upper_a upper_b)

  -- | Returns the intersection of two intervals. The intervals must have a
  -- non-null intersection.
  unsafe_intersection :: (Ord a) => Interval a -> Interval a -> Interval a
  unsafe_intersection = (fromJust .) . intersection

  jsonify :: (Show a) => Interval a -> [Char]
  jsonify (Interval a b) =
      "{low: " ++ (show a) ++ ", high: " ++ (show b) ++ "}"
