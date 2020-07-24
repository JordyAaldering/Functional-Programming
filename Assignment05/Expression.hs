
-- Exercise 5.3
-- Student names and numbers:
-- Thomas van Harskamp, s1007576
-- Jordy Aaldering, s1004292

module Expression
where
import Prelude hiding (fail)
import Parser

infixl 6 :+:
infixl 7 :*:

data Expr
  =  Lit Integer
  |  Expr :+: Expr
  |  Expr :*: Expr
  deriving (Show)

expr, term, factor :: Parser Expr
expr   = term
      .| term   >>= \i -> symbol '+' >> expr >>= \j -> return (i :+: j)
term   = factor
      .| factor >>= \i -> symbol '*' >> term >>= \j -> return (i :*: j)
factor = many digit >>= \i                          -> return (Lit (read i))
      .| (symbol '(' >> expr >>= \i -> symbol ')'   >> return i)

expr2, term2, factor2 :: Parser Expr
expr2   = do term2
       .| do i <- term2;   symbol '+'; j <- expr2; return (i :+: j)
term2   = do factor2
       .| do i <- factor2; symbol '*'; j <- term2; return (i :*: j)
factor2 = do i <- many digit;                      return (Lit (read i))
       .| do symbol '('; i <- expr2 ; symbol ')';  return i

-- Exercise 5.3.1
expr3 :: Parser Expr
expr3 = do i <- many digit; return (Lit (read i))
     .| do i <- expr3; symbol '+'; j <- expr3; return (i :+: j)
     .| do i <- expr3; symbol '*'; j <- expr3; return (i :*: j)
     .| do symbol '('; i <- expr3; symbol ')'; return i

-- Exercise 5.3.2
-- The running time improves noticably.
expr4, term4, factor4 :: Parser Expr
expr4   = do i <- term4;   alt [return i, do symbol '+'; j <- expr4; return (i :+: j)]
term4   = do i <- factor4; alt [return i, do symbol '*'; j <- term4; return (i :*: j)]
factor4 = do i <- many digit;                                        return (Lit (read i))
