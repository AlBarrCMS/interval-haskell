{-|
Module       : Polynomial
Description  : General polynomials which can be differentiated
 -
 -}
module Polynomial(
    Polynomial (Polynomial, Product, Power, Sum),
    Term (Term),
    Atom (Atom),
    to_const,
    const_poly,
    x_poly,
    construct_poly,
    construct_univariate_polynomial,
    replace_variable,
    evaluate,
    evaluate_one_var,
    variables_in_poly,
    taylor_expand,
    partials,
    differentiate,
    gradient,
    hessian,
    add,
    scalar_multiply,
    multiply,
    reduce_full,
    add_full,
    multiply_full,
    reduce_sums
) where

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Set as Set

-- TODO: Fix squaring

-- | A variable raised to a power. Used to form polynomial Terms
data Atom 
    = Atom Char Int deriving Eq

-- | A term in a polynomial
data Term a
    = Term a [Atom] deriving Eq

-- | A polynomial
data Polynomial a
    -- | A plain polynomial
    =   Polynomial [Term a]

    -- | A polynomial expressed as a product of polynomials
    | Product (Polynomial a) (Polynomial a)

    -- | A polynomial expressed as a power of a polynomial
    | Power (Polynomial a) Int

    -- | A polynomial expressed as a sum of polynomials
    | Sum (Polynomial a) (Polynomial a) deriving Eq

instance Show Atom where
    show (Atom var 1) = [var]
    show (Atom var pow) = var : ("^" ++ (show pow))
instance (Show a) => Show (Term a) where
    show (Term coeff vars) = (show coeff) ++ concat (map show vars)
instance (Show a) => Show (Polynomial a) where
    show (Polynomial []) = "0"
    show (Polynomial terms) = intercalate " + " (map show terms)
    show (Product a b) = "(" ++ (show a) ++ ") * (" ++ (show b) ++ ")"
    show (Sum a b) = (show a) ++ " + " ++ (show b)
    show (Power a n) = "(" ++ (show a) ++ ")^" ++ (show n) 

instance Ord Atom where
    compare (Atom a n_a) (Atom b n_b) = if cmp /= EQ then cmp else compare n_a n_b
        where cmp = compare a b
instance(Ord a) => Ord (Term a) where
    compare (Term _ a_atoms) (Term _ b_atoms) = compare a_atoms b_atoms

instance (Num a, Ord a) => Num (Polynomial a) where
    (+) = add
    (*) = multiply
    negate poly = scalar_multiply (-1) poly
    signum poly = Polynomial [Term 1 []]
    abs = id
    fromInteger n = Polynomial [Term (fromInteger n) []]

instance Functor (Term) where
    fmap f (Term coeff atoms) = Term (f coeff) atoms

instance Functor (Polynomial) where
    fmap f (Polynomial terms) = Polynomial (map (fmap f) terms)
    fmap f poly = pmap (fmap f) poly

-- | Takes a function and either applies it to a stand-along polynomial or
-- | applies it to the constituent polynomials in a sum, product or power
pmap :: (Polynomial a -> Polynomial b) -> Polynomial a -> Polynomial b
pmap f p@(Polynomial _) = f p
pmap f (Product a b) = Product (f a) (f b)
pmap f (Sum a b) = Sum (f a) (f b)
pmap f (Power a n) = Power (f a) n

-- | Compose a list of functions and apply them to an argument.
-- | compose [f, g] x = g(f(x))
compose :: [a -> a] -> a -> a
compose fns v = foldl (flip (.)) id fns $ v

-- | Construct a constant polynomial
const_poly :: a -> Polynomial a
const_poly n = Polynomial [Term n []]

-- | Returns the constant term of the polynomial
to_const :: (Num a, Ord a) => Polynomial a -> a
to_const (Product a b) = (to_const a) * (to_const b)
to_const (Sum a b) = (to_const a) + (to_const b)
to_const (Power a n) = (to_const a)^n
to_const poly = const_term reduced_poly
    where
        reduced_poly = reduce_full poly
       
        -- Returns the constant term of a polynomial assumed to be 
        -- in standard form (so either the constant is the first term,
        -- or there is no constant 
        const_term :: (Num a) => Polynomial a -> a
        const_term (Polynomial (Term c []:_)) = c
        const_term _ = 0

-- | Construct a polynomial in x from a list of coefficients
x_poly :: (Num a) => [a] -> Polynomial a
x_poly = construct_univariate_polynomial 'x'

-- | Construct a polynomial from a string. The parsing is very basic so every
-- | term needs a coefficient (even if it's 1) and a power of x (even if it's x^0 
-- | or x^1)
construct_poly :: (Num a, Read a, Ord a) => String -> Polynomial a
construct_poly string_rep = reduce_full $ Polynomial $ map to_poly_term terms
    where
        to_poly_term term_string = Term (get_coeff term_string) (get_vars term_string)
        
        get_coeff :: (Read a) => String -> a
        get_coeff string = read $ takeWhile isDigit string

        get_vars :: String -> [Atom]
        get_vars [] = []
        get_vars string = (Atom var exp) : (get_vars rest)
            where
                non_coeff = dropWhile isDigit string 
                var = head non_coeff
                exp = read $ takeWhile isDigit (tail $ tail non_coeff)
                rest = dropWhile isDigit (tail $ tail non_coeff)
        terms = map (filter (/= ' ')) $ splitOn "+" string_rep

-- | Construct a polynomial in one arbitrary variable from a list of coefficients
construct_univariate_polynomial :: (Num a) => Char -> [a] -> Polynomial a
construct_univariate_polynomial var coeffs =
    Polynomial $ zipWith to_term [0 .. length coeffs] coeffs 
        where
            to_term :: Int -> a -> Term a
            to_term 0 coeff = Term coeff []
            to_term pow coeff = Term coeff [Atom var pow] 

-- | Replace a variable in a polynomial with another polynomial
replace_variable :: (Eq a, Num a, Ord a) =>
    Char -> Polynomial a -> Polynomial a -> Polynomial a
replace_variable var replacement (Polynomial terms) = sum $ map (replace_variable_term var replacement) terms 
    where
        replace_variable_term :: (Num a) => Char -> Polynomial a -> Term a -> Polynomial a
        replace_variable_term var replacement term@(Term coeff atoms)
            | contains_term = Product (new_poly) (Power replacement pow)
            | otherwise     = Polynomial [ term ]
                where
                    contains_term = or $ map (has_var var) atoms
                    (Atom _ pow) = head $ filter (has_var var) atoms 
                    remaining_atoms = filter (not_has_var var) atoms
                    new_poly = Polynomial [ Term coeff remaining_atoms ]
replace_variable var replacement poly = pmap (replace_variable var replacement) poly



-- | Evaluate a polynomial with given values for some of its variables
evaluate :: (Eq a, Num a, Ord a) => [Char] -> [a] -> Polynomial a -> Polynomial a
evaluate vars vals poly = compose evaluations poly
    where
        evaluations = zipWith (\var val -> evaluate_one_var var val) vars vals
        

-- | Evaluate a polynomial at var = val
evaluate_one_var :: (Eq a, Num a, Ord a) => Char -> a -> Polynomial a -> Polynomial a
evaluate_one_var var val (Polynomial terms) =
    reduce_sums $ Polynomial (map (evaluate_term var val) terms)
    where
        evaluate_term :: (Eq a, Num a) => Char -> a -> Term a -> Term a
        evaluate_term var val term = Term new_coeff irrelevant_atoms
            where
                reduced_term@(Term coeff atoms) = reduce_term term
                irrelevant_atoms = filter (not_has_var var) atoms
                relevant_atoms = filter (has_var var) atoms
                 
                evaluate_atom :: (Num a) => Atom -> a -> a
                evaluate_atom (Atom _ pow) val = val^pow 

                new_coeff = if length relevant_atoms > 0 then
                                coeff * (evaluate_atom (head relevant_atoms) val)
                            else coeff
evaluate_one_var var val poly = pmap (evaluate_one_var var val) poly

-- | Return a list of the variables used in a polynomial
variables_in_poly :: Polynomial a -> [Char]
variables_in_poly poly = Set.toList $ variable_set poly

-- | Return a set of the variables used in a polynomial
variable_set :: Polynomial a -> Set.Set Char
variable_set (Product a b) = Set.union (variable_set a) (variable_set b)
variable_set (Sum a b)     = Set.union (variable_set a) (variable_set b)
variable_set (Power a _)   = variable_set a
variable_set (Polynomial terms) = Set.unions $ map variables_in_term terms
    where
        variables_in_term :: Term a -> Set.Set Char
        variables_in_term (Term _ atoms) = Set.fromList $ map (\(Atom var _) -> var) atoms

-- | Returns the a list of partial derivatives of a polynomial with respect to the
-- speficied variables. The derivatives are returned in the order the variables
-- were given in.
gradient :: (Num a, Ord a) => [Char] -> Polynomial a -> [Polynomial a]
gradient vars poly = sequence (map differentiate vars) poly

-- | Returns a matrix of second derivatives of a polynomial with respect to the
-- specified variables. Since polynomials are infinitely differentiable, the second
-- partial derivatives commute, so this matrix is symmetric (and thus can be viewed as
-- being in row-major or column-major order). The order of the derivatives along each 
-- dimension of the matrix is the same as the order of the input variables.
hessian :: (Num a, Ord a) => [Char] -> Polynomial a -> [[Polynomial a]]
hessian vars poly = map (gradient vars) (gradient vars poly)

-- | If we write x as (c + (x-c)), we can find the taylor expansion of p(x) by writing
-- | p(c + (x-c)) and expanding. We use this technique to find the taylor expansion of
-- | a polynomial 
taylor_expand :: (Num a, Ord a) => [Char] -> [a] -> Polynomial a -> Polynomial a
taylor_expand vars center poly = compose undo_replacements new_poly
    where
        undo_replacements = zipWith3 (\temp_var var pt ->
            replace_variable temp_var (Polynomial [Term 1 [Atom var 1], Term (-pt) []]))
            temp_vars vars center
        new_poly = reduce_full $ compose replacement_commands $ reduce_full poly
        replacement_commands =
            zipWith (\var poly -> replace_variable var poly) vars replacements
        replacements = zipWith (\n var -> Polynomial [Term n [], Term 1 [Atom var 1]])
                        center temp_vars
        temp_vars = take (length vars) free_vars
        free_vars = filter (\v -> Set.notMember v used_vars) ['a', 'b' .. ]
        used_vars = variable_set poly

-- | Return a list of the mixed partial derivatives used in a taylor approximation
-- | of degree deg
taylor_partials :: (Num a, Ord a) => [Char] -> Int -> Polynomial a -> [[Polynomial a]]
taylor_partials vars deg poly = take (deg + 1) $ unfoldr (\derivs -> Just (derivs, concat $ map (partials vars) derivs)) [poly]

-- | Return the partial derivatives with return to the given variables
partials :: (Num a, Ord a) => [Char] -> Polynomial a -> [Polynomial a]
partials vars poly = map (\var -> differentiate var poly) vars 

-- | Differentiate a polynomial with respect to a variable
differentiate :: (Num a, Ord a) => Char -> Polynomial a -> Polynomial a
differentiate var (Sum a b) = (differentiate var a) + (differentiate var b)
differentiate var (Product a b) =
    ((differentiate var a) * b) + (a * (differentiate var b))
differentiate var (Power a n) =
    (scalar_multiply (fromIntegral n) (Power a (n-1))) * (differentiate var a)
differentiate var (Polynomial terms) =
    reduce_sums $ Polynomial (map (differentiate_term var) terms)

differentiate_term :: (Num a) => Char -> Term a -> Term a
differentiate_term var (Term coeff atoms) =
    if contains_var then
        Term (new_coeff) (diffed_term : filter (not_has_var var) atoms) else Term 0 []
            where
                filtered_list = filter (has_var var) atoms
                contains_var = not $ null $ filtered_list                
                Atom _ pow = head filtered_list
                diffed_term = Atom var (pow - 1)
                new_coeff = (fromIntegral pow) * coeff

-- | Returns true if an atom is not a power of var
not_has_var :: Char -> Atom -> Bool
not_has_var var atom = not $ has_var var atom

-- | Returns true if an atom is a power of var
has_var :: Char -> Atom -> Bool
has_var var (Atom a_var _) = var == a_var

-- | Return the formal sum of two polynomials
add :: (Num a, Ord a) => Polynomial a -> Polynomial a -> Polynomial a
add 0 a = a
add a 0 = a
add a@(Polynomial _) b@(Polynomial _) = reduce_sums $ Sum a b
add a b = Sum a b

-- | Multiply a polynomial by a constant
scalar_multiply :: (Num a, Ord a) =>  a -> Polynomial a -> Polynomial a
scalar_multiply s (Polynomial terms) = Polynomial (map (fmap (*s)) terms)
scalar_multiply s (Product a b) = (scalar_multiply s a) * b
scalar_multiply s (Sum a b) = (scalar_multiply s a) + (scalar_multiply s b)
scalar_multiply s p@(Power _ _) = (const_poly s) * p

-- | Return the formal product of two polynomials
multiply :: (Eq a) => Polynomial a -> Polynomial a -> Polynomial a
multiply a b | a == b = Power a 2
multiply (Power a n) b | a == b = Power a (n+1)
multiply (Power a n) (Power b m) | a == b = Power a (n + m)
multiply a b = Product a b

-- | Return the expanded form of the polynomial
reduce_full :: (Num a, Ord a, Eq a) => Polynomial a -> Polynomial a
reduce_full p@(Polynomial terms) = reduce_sums p
reduce_full (Sum a 0) = a
reduce_full (Sum 0 a) = a
reduce_full (Sum a b) = add_full a b
reduce_full (Product 0 _) = 0
reduce_full (Product _ 0) = 0
reduce_full (Product a 1) = a
reduce_full (Product 1 a) = a
reduce_full (Product a b) = multiply_full a b
reduce_full (Power a n) = reduce_full $ foldl' multiply_full a $ take (n-1) $ repeat a 

-- | Return the expanded sum of polynomials
add_full :: (Num a, Ord a) => Polynomial a -> Polynomial a -> Polynomial a
add_full (Polynomial a) (Polynomial b) = reduce_sums $ Polynomial (a ++ b)
add_full a b = add_full (reduce_full a) (reduce_full b)

-- | Return the expanded product of polynomials
multiply_full :: (Num a, Ord a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
multiply_full (Polynomial terms) poly@(Polynomial _) = foldl' add 0 $ map (multiply_by_term poly) terms
multiply_full a b = multiply_full (reduce_full a) (reduce_full b)

-- | Multiply out a polynomial times a term of a polynomial
multiply_by_term :: (Num a, Ord a) => Polynomial a -> Term a -> Polynomial a
multiply_by_term (Polynomial terms) term = Polynomial (map (multiply_terms term) terms)

-- | Multiply two polynomial terms together
multiply_terms :: (Num a, Eq a) => Term a -> Term a -> Term a
multiply_terms (Term coeff_a atoms_a) (Term coeff_b atoms_b) = 
    Term (coeff_a * coeff_b) (atoms_a ++ atoms_b)

-- | Sums together terms with the same variables. Does not multiply out products
reduce_sums :: (Num a, Ord a, Eq a) => Polynomial a -> Polynomial a
reduce_sums (Sum (Polynomial a_terms) (Polynomial b_terms)) =
    reduce_sums $ Polynomial (a_terms ++ b_terms)
reduce_sums (Polynomial []) = 0
reduce_sums (Polynomial terms) = 
    Polynomial (filter is_nonzero $ compress []
        (head sorted_reduced_terms) (tail sorted_reduced_terms))
        where     
            sorted_reduced_terms = reverse $ sort $ map reduce_term terms 

            compress :: (Num a) => [Term a] -> Term a -> [Term a] -> [Term a]
            compress acc curr [] = curr : acc
            compress acc curr (x:xs) = if can_sum_terms curr x then 
                                          compress acc (sum_terms curr x) xs
                                       else compress (curr:acc) x xs

            is_nonzero :: (Num a, Eq a) => Term a -> Bool 
            is_nonzero (Term coeff _) = coeff /= 0

            can_sum_terms :: Term a -> Term a -> Bool
            can_sum_terms (Term _ a_atoms) (Term _ b_atoms) = a_atoms == b_atoms

            -- assumes the terms have the same variables with the same powers so their
            -- sum is the sum of the coefficients times the variables of one
            -- 6x^2y + 7x^2y = 13x^2y
            sum_terms :: (Num a) => Term a -> Term a -> Term a
            sum_terms (Term coeff_a atoms) (Term coeff_b _) =
                Term (coeff_a + coeff_b) atoms
reduce_sums poly = pmap reduce_sums poly

-- | Groups variables together in a term of a polynomial
reduce_term :: (Num a, Eq a) => Term a -> Term a
reduce_term (Term 0 atoms) = Term 0 []
reduce_term (Term coeff atoms) = Term coeff grouped_atoms
    where
        grouped_atoms = foldl' group_same [] sorted_atoms
        
        group_same :: [Atom] -> Atom -> [Atom]
        group_same [] atom = [atom]
        group_same lst@(a@(Atom _ na):xs) b@(Atom var nb) = 
             if same_var a b then (Atom var (na + nb)):xs else b : lst

        sorted_atoms = reverse $ sort $ filter is_nontrivial_atom atoms

        is_nontrivial_atom :: Atom -> Bool
        is_nontrivial_atom (Atom var n) = n > 0

        same_var :: Atom -> Atom -> Bool
        same_var (Atom a _) (Atom b _) = a == b
