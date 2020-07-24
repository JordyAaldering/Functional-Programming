
-- Exercise 5.4
-- Student names and numbers:
-- Thomas van Harskamp, s1007576
-- Jordy Aaldering, s1004292

module Lambda
where
import Prelude hiding (fail)
import Parser

infixl 9 :@

data Lambda var
  = Var var
  | Fun var (Lambda var)
  | Lambda var :@ Lambda var
  deriving (Show)

-- Exercise 5.4.1
-- lambda := term
--         | '\' var '->' lambda
--         | 'λ' var '.'  lambda
-- term  := var | '(' lambda ')'
-- var   := idStart idMid
-- start := lower   | upper | '_'
-- mid   := idStart | digit | "'"

-- Exercise 5.4.2
lambda :: Parser (Lambda String)
lambda = (many (term  >>= \x -> symbol ' ' >> return x) >>= \ys -> term   >>= \y -> return (foldr (:@) y ys))
      .| (symbol '\\' >> var >>= \x -> symbol '-' >> symbol '>' >> lambda >>= \y -> return (Fun x y))
      .| (symbol 'λ'  >> var >>= \x -> symbol '.' >>               lambda >>= \y -> return (Fun x y))

term :: Parser (Lambda String)
term = (var >>= \x -> return (Var x))
    .| (symbol '(' >> lambda >>= \x -> symbol ')' >> return x)

var :: Parser String
var = start >>= \x -> many mid >>= \y -> return (x:y)

start, mid :: Parser Char
start = lower .| upper .| symbol '_'
mid   = start .| digit .| symbol '\''

