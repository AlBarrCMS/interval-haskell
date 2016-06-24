{-|
Module      : PolynomialParser
Description : A parser to parse polynomials in string form
-}
module PolynomialParser (
    parse_polynomial
) where

import Control.Applicative
import qualified Data.Char as Char
import qualified Text.Parsec as Parsec
import Polynomial

term_separator :: Parsec.Parsec String () String
term_separator = Parsec.string "+"

-- Adds a 1 after all negative signs not followed by a digit so the parser can
-- interpret the negative sign as part of a coefficient. E.g. the parser would
-- not be able to parse x^2-y, so we turn it into x^2-1y
fix_negative_signs :: String -> String
fix_negative_signs (x:y:zs)
  | x == '-' && not (Char.isDigit y) = '-' : '1' : y : fix_negative_signs zs
  | x == '-'  = x : y : fix_negative_signs zs
  | otherwise = x : fix_negative_signs (y:zs)
fix_negative_signs x = x

-- | Reads in a string and returns a Polynomial if it parsed correctly
-- and Nothing if the parser failed
parse_polynomial :: (Num a, Read a) => String -> Maybe (Polynomial a)
parse_polynomial poly_string = right_to_maybe parse_result
    where
        stripped_poly = fix_negative_signs $ filter (/= ' ') poly_string
        parse_result = Parsec.parse polynomial_parser stripped_poly stripped_poly
        right_to_maybe :: Either a b -> Maybe b
        right_to_maybe (Left _) = Nothing
        right_to_maybe (Right val) = Just val

polynomial_parser :: (Num a, Read a) => Parsec.Parsec String () (Polynomial a)
polynomial_parser = do
    terms <- Parsec.many $ do
        term <- term_parser 
        Parsec.many term_separator
        return term
    return $ Polynomial terms

term_parser :: (Num a, Read a) => Parsec.Parsec String () (Term a)
term_parser = term_parser_nontrivial_coeff <|> term_parser_nontrivial_var

term_parser_nontrivial_coeff :: (Num a, Read a) => Parsec.Parsec String () (Term a)
term_parser_nontrivial_coeff = do
    Parsec.try $ do
        coeff <- Parsec.many1 (Parsec.oneOf "0123456789.-e[]")
        atoms <- Parsec.many atom_parser
        return $ Term (read coeff) atoms

term_parser_nontrivial_var :: (Num a, Read a) => Parsec.Parsec String () (Term a)
term_parser_nontrivial_var = do
    Parsec.try $ do
        coeff <- Parsec.option "1" $ Parsec.many1 (Parsec.oneOf "0123456789.-e[]")
        atoms <- Parsec.many1 atom_parser
        return $ Term (read coeff) atoms
    

atom_parser :: Parsec.Parsec String () Atom
atom_parser = do
    var <- Parsec.letter
    pow <- Parsec.option "1" power_parser
    return $ Atom var (read pow)

power_parser :: Parsec.Parsec String () String
power_parser = do
    Parsec.char '^'
    pow <- Parsec.many Parsec.digit
    return pow
