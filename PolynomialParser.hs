module PolynomialParser

where

import Control.Applicative
import qualified Text.Parsec as Parsec
import Polynomial

term_separator :: Parsec.Parsec String () String
term_separator = Parsec.string "+"

parse_polynomial :: (Num a, Read a) => String -> Maybe (Polynomial a)
parse_polynomial poly_string = right_to_maybe parse_result
    where
        stripped_poly = filter (/= ' ') poly_string
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
