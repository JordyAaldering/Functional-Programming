
-- Exercise 5.5
-- Student names and numbers:
-- Thomas van Harskamp, s1007576
-- Jordy Aaldering, s1004292

module SKI
where
import Lambda

data SKI var
  = Free var
  | S
  | K
  | I'
  | App (SKI var) (SKI var)

i :: env -> env
i arg = arg

k :: a -> (env -> a)
k x _arg = x

s :: (env -> a -> b) -> (env -> a) -> (env -> b)
s x y arg = (x arg) (y arg)

-- Exercise 5.5.1
-- abstr :: (Eq var) => var -> SKI var -> SKI var
-- abstr 

-- Exercise 5.5.2
-- compile :: (Eq var) => Lambda var -> SKI var
-- compile 

-- Exercise 5.5.3
-- reduce :: SKI var -> [SKI var] -> SKI var
-- reduce 

-- Exercise 5.5.4
-- ...

-- twice = parse expr "λf.λx.f(fx)"
-- twice
-- compile twice
-- reduce it [Free 's', Free 'z']

-- compile (twice :@ twice)
-- reduce it [Free 's', Free 'z']

-- parse expr "(λx.xx)(λx.xx)"
-- compile it
-- reduce it []
-- parse expr "λf.(λx.f(xx))(λx.f(xx))"
-- compile it
-- reduce it [Free 's', Free 'z']
