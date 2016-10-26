{-|
  Module       : ExpressionParser
  Description  : Parsec parser for Expression objects
 -}
module ExpressionParser(
) where
  import Expression

  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Number

  expression :: Parser Expression
  expression = terms <|> factors <|> exps <|> atom

  terms :: Parser Expression
  terms = (sepBy1 (factors <|> exps <|> atom)
                  (spaces >> char '+' >> spaces) >>= return . Expression "(+)")
      <|> (sepBy1 (factors <|> exps <|> atom)
                  (spaces >> char '-' >> spaces) >>= return . Expression "(-)")

  factors :: Parser Expression
  factors = (sepBy1 (factors <|> exps <|> atom)
                    (spaces >> optional (char '*')
                              >> spaces) >>= return . Expression "(*)")
      <|> (sepBy1 (factors <|> exps <|> atom)
                  (spaces >> char '/' >> spaces) >>= return . Expression "(/)")

  exps :: Parser Expression
  exps = sepBy1 (factors <|> exps <|> atom)
                (spaces >> optional (char '^')
                          >> spaces) >>= return . Expression "(^)"

  atom :: Parser Expression
  atom = func <|> var <|> imm

  func :: Parser Expression
  func =
    do
      op <- option "(+)" (many1 lower)
      arg <- between (char '(') (char ')') expression
      return (Expression op [arg])

  var :: Parser Expression
  var = lower >>= return . Var

  imm :: Parser Expression
  imm = natFloat >>= return . Imm
