
Exercises 11.1 & 11.2
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292

> module Evaluator
> where
> import Data.Maybe

> infixl 6 :+:
> infixl 7 :*:
> infixr 1 :?:

> data Expr
>   = Lit Integer
>   | Expr :+: Expr
>   | Expr :*: Expr
>   | Div Expr Expr
>   | Expr :?: Expr
>   | Var String

> evalA :: (Applicative f) => Expr -> f Integer
> evalA (Lit i)     = pure i
> evalA (e1 :+: e2) = pure (+) <*> evalA e1 <*> evalA e2
> evalA (e1 :*: e2) = pure (*) <*> evalA e1 <*> evalA e2
> evalA (Div e1 e2) = pure div <*> evalA e1 <*> evalA e2

> toss :: Expr
> toss = Lit 0 :?: Lit 1


> -- Exercise 11.1 --
> evalN :: Expr -> [Integer]
> evalN (Lit i)     = pure i
> evalN (e1 :+: e2) = pure (+) <*> evalN e1 <*> evalN e2
> evalN (e1 :*: e2) = pure (*) <*> evalN e1 <*> evalN e2
> evalN (Div e1 e2) = pure div <*> evalN e1 <*> evalN e2
> evalN (e1 :?: e2) = evalN e1 ++  evalN e2

evalN toss
[0,1]
evalN (toss :+: Lit 2 :*: toss)
[0,2,1,3]
evalN (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: toss)))
[0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15]


> -- Exercise 11.2.1 --
> evalR :: Expr -> [(String, Integer)] -> Integer
> evalR (Lit i)     = pure i
> evalR (e1 :+: e2) = pure (+) <*> evalR e1 <*> evalR e2
> evalR (e1 :*: e2) = pure (*) <*> evalR e1 <*> evalR e2
> evalR (Div e1 e2) = pure div <*> evalR e1 <*> evalR e2
> evalR (Var v)     = pure (fromMaybe 0)    <*> lookup v

evalR (Var "a" :+: Lit 1) [("a", 4711), ("b", 0815)]
4712
evalR (Var "a" :*: Var "b") [("a", 4711), ("b", 0815)]
3839465
evalR (Var "a" :*: Var "c") [("a", 4711), ("b", 0815)]
0

