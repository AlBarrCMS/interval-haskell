{-|
  Module       : Expression
  Description  : Functions that can be handled by the RIN
 -}
module Expression(
  Expression (Expression, Var, Imm)
) where
  data Expression = Expression String [Expression]
                  | Var Char
                  | Imm (Either Integer Double)
