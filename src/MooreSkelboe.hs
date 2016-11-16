{-|
  Module      : MooreSkelboe
  Description : Code to find the minima of a polynomial using Moore Skelboe Optimization
 -}

module MooreSkelboe (
  minimize,
  leaf_minimize,
  heuristic_leaf_minimize,
  moore_skelboe_write_leaf_data
) where

  import Data.List
  import Data.Ord

  import Debug.Trace

  import GradientDescent
  import Interval
  import Polynomial
  import qualified Data.Sequence as S

  -- | Computes the inclusion function of a polynomial applied to a given interval by
  -- applying the polynomial to integer arguments. If any variables of the polynomial
  -- are not specified, they are assumed to be 0.
  inclusion :: (Eq a, Ord a, Num a)
            => Polynomial a  -- ^ The polynomial whose inclusion function we
                             -- want to find
            -> [Char]        -- ^ The variables in the polynomial
            -> [Interval a]  -- ^ The intervals over which the inclusion is to
                             -- be calculated
            -> Interval a    -- ^ The interval given by the inclusion function
  inclusion poly vars domains = to_const $ evaluate vars domains (fmap from_num poly)

  -- Divides a region in half along its longest edge.
  simple_subdivide :: (Num a, Ord a, Fractional a, Show a)
                   => [Interval a]                  -- Input region
                   -> ([Interval a], [Interval a])  -- (Lower sub-region, Upper sub-region)
  simple_subdivide region = (lower_region, higher_region)
    where
      side_lengths = map width region

      -- The index of the longest side of the region
      max_index = snd $ maximumBy (comparing fst) (zip side_lengths [0..])

      -- The interval representing the longest side
      split_interval@(Interval low high) = region !! max_index
      lower_interval  = Interval low (midpoint split_interval)
      higher_interval = Interval (midpoint split_interval) high
      lower_region    = replaceAtIndex max_index region lower_interval
      higher_region   = replaceAtIndex max_index region higher_interval

      -- Replaces the nth element of list ls with item
      replaceAtIndex n ls item = a ++ (item:b)
        where
          (a, (_:b)) = splitAt n ls

  generic_minimize :: (Num a, Ord a, RealFrac a, Floating a, Show a)
      => ([Interval a]
      -> Interval a
      -> a
      -> Bool
      -> [b])          -- Function to bundle up returned data
      -> Polynomial a  -- Polynomial whose minima we want to find
      -> [Char]        -- Variables in polynomial
      -> a             -- The maximum size of a solution region
      -> a             -- The current estimate on the upper bound of the minimum
      -> [Interval a]  -- The region to search in
      -> ([b], a)      -- The returned data and current minimum estimate
  generic_minimize bundle_data poly vars max_soln_size sup_min region
      | not is_potential_minimum = (bundle_data region inclusion_function sup_min False, sup_min)
      | small_enough_region = (bundle_data region inclusion_function new_sup_min True, new_sup_min)
      | otherwise = combined_subregion_solutions
    where
      inclusion_function@(Interval lower_bound upper_bound) = inclusion poly vars region
      is_potential_minimum = sup_min >= lower_bound
      new_sup_min = min sup_min upper_bound

      {-small_enough_region = trace ((show region)++(show inclusion_function) ++ (show sup_min)) wdth < max_soln_size--maximum (map width region) < max_soln_size-}
      {-wdth = maximum (map width region)-}
      small_enough_region = maximum (map width region) < max_soln_size

      (lower_region, upper_region) = simple_subdivide region
      (lower_soln, lower_sup_min) = generic_minimize bundle_data
                                                     poly
                                                     vars
                                                     max_soln_size
                                                     new_sup_min
                                                     lower_region
      (upper_soln, upper_sup_min) = generic_minimize bundle_data
                                                     poly
                                                     vars
                                                     max_soln_size
                                                     lower_sup_min
                                                     upper_region
      combined_subregion_solutions = (lower_soln ++ upper_soln, upper_sup_min)


  heuristic_minimize :: (Num a, Ord a, RealFrac a, Floating a, Show a)
      => ([Interval a]
      -> Interval a
      -> a
      -> Bool
      -> [b])          -- Function to bundle up returned data
      -> Polynomial a  -- Polynomial whose minima we want to find
      -> [Char]        -- Variables in polynomial
      -> a             -- The maximum size of a solution region
      -> a             -- The current estimate on the upper bound of the minimum
      -> [Interval a]  -- The region to search in
      -> [Interval a]  -- The entire search space
      -> ([b], a)      -- The returned data and current minimum estimate
  heuristic_minimize bundle_data poly vars max_soln_size sup_min region search_space
      | not is_potential_minimum = (bundle_data region inclusion_function sup_min False, sup_min)
      | small_enough_region = (bundle_data region inclusion_function sup_min True, new_sup_min)
      | otherwise = combined_subregion_solutions
    where
      inclusion_function@(Interval lower_bound upper_bound) = inclusion poly vars region
      is_potential_minimum = sup_min >= lower_bound
      new_sup_min = compute_new_min sup_min upper_bound
        where
          {-compute_new_min current_min new_potential_min = min current_min new_potential_min-}
          compute_new_min current_min new_potential_min
            | current_min < new_potential_min = current_min
            {-| otherwise = trace ("D: " ++ (show [new_potential_min, l]) ++show inclusion_function++ (show (map corner region) ++ (show region))) min new_potential_min l-}
                {-where l = bounded_gradient_descent 0.01 vars search_space 10 poly (map corner region)-}
            | otherwise = min new_potential_min l
                where l = bounded_gradient_descent 0.01 vars search_space 10 poly (map corner region)

      small_enough_region = maximum (map width region) < max_soln_size

      (lower_region, upper_region) = simple_subdivide region
      (lower_soln, lower_sup_min) = heuristic_minimize bundle_data
                                                       poly
                                                       vars
                                                       max_soln_size
                                                       new_sup_min
                                                       lower_region
                                                       search_space
      (upper_soln, upper_sup_min) = heuristic_minimize bundle_data
                                                       poly
                                                       vars
                                                       max_soln_size
                                                       lower_sup_min
                                                       upper_region
                                                       search_space
      combined_subregion_solutions = (lower_soln ++ upper_soln, upper_sup_min)

  count_termination_minimize = generic_minimize (\_ _ _ end_case -> [end_case])

  -- | Returns a list of regions that could contain zeros of a polynomial.
  -- These regions are computed using the remainder interval newton method.
  minimize :: (Num a, Ord a, RealFrac a, Floating a, Show a)
      => Polynomial a         -- The polynomial whose zeros we want
      -> [Char]               -- The variables in the polynomial
      -> a                    -- The maximum size of a solution region
      -> a                    -- The current estimate on the upper bound of the minimum
      -> [Interval a]         -- The region to search in
      -> ([[Interval a]], a)  -- The acceptable solution regions, and our bound on the min
  minimize = generic_minimize (\region _ _ is_soln ->
      if is_soln then
        [region]
      else
        [])


  -- | Returns the result of running remainder interval newton. Returns a list of
  -- tuples. The first component is a region, and the second is the value of the inclusion
  -- function on that region, and the third is whether or not it is a potential minimum
  leaf_minimize :: (Num a, Ord a, RealFrac a, Floating a, Show a)
      => Polynomial a                        -- The polynomial whose zeroes we want
      -> [Char]                              -- The variables in the polynomial
      -> a                                   -- The maximum size of a solution region
      -> a                                   -- The current estimate on the upper bound
                                             --     of the minimum
      -> [Interval a]                        -- The region to search in
      -> [([Interval a], Interval a, a, Bool)]  -- The solution regions and rejected
                                             -- regions
  leaf_minimize poly vars max_soln_size sup_min region = fst $
          generic_minimize (\region inclusion min is_soln -> [(region, inclusion, min, is_soln)])
                           poly
                           vars
                           max_soln_size
                           sup_min
                           region

  heuristic_leaf_minimize :: (Num a, Ord a, RealFrac a, Floating a, Show a)
      => Polynomial a                             -- The polynomial whose zeroes we want
      -> [Char]                                   -- The variables in the polynomial
      -> a                                        -- The maximum size of a solution region
      -> a                                        -- The current estimate on the upper bound
                                                  --     of the minimum
      -> [Interval a]                             -- The region to search in
      -> ([([Interval a], Interval a, a, Bool)], a)  -- The solution regions and rejected
                                                  -- regions. The second component is the minimum
  heuristic_leaf_minimize poly vars max_soln_size sup_min region =
          heuristic_minimize (\region inclusion min is_soln -> [(region, inclusion, min, is_soln)])
                           poly
                           vars
                           max_soln_size
                           sup_min
                           region
                           region


  -- | Returns a string containing the information from the leaves.
  -- Leaves are separated by newlines. Each line contains the low value of the
  -- leaf's interval, the high value of the leaf's interval, and the low and
  -- high values of the boundaries in each dimension. All values are separated
  -- by commas.
  moore_skelboe_write_leaf_data :: (Num a, Fractional a, RealFrac a, Show a)
                      => [([Interval a], Interval a, a, Bool)] -> String
  moore_skelboe_write_leaf_data leaves = concat (map write_leaf leaves)
    where
      write_leaf (input, output, min, is_soln) =
          "{\"input\": " ++ bound_list input ++ ", \"output\": " ++
          jsonify output ++ ", \"min\": " ++ (show min) ++ "}\n"
        where
          -- List of lower an upper bounds of leaf regions
          bound_list = (\a -> "[" ++ a ++ "]") . intercalate ", " . map jsonify
