-- Var means String so variable name like "x" is text
type Var = String

-- Represents lambda calculus terms and Term is one of 3 things
data Term
  = Variable Var -- a name like "x"
  | Lambda Var Term -- Var is input name and Term is body Lambda "x" (Variable "x") is \x. x.
  | Apply Term Term -- Apply Term to Term like f x

-- deriving Show

instance Show Term where
  show = pretty

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Variable "a")) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 0
  where
    f i (Variable x) = x
    f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m
    f i (Apply n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m

------------------------- Assignment 1

-- Takes a whole number and returns a term
numeral :: Int -> Term
-- Build the fixed wrapper lamba f containing lambda x
-- and the body depending on the input num
numeral i = Lambda "f" (Lambda "x" (body i))
  where
    body 0 = Variable "x" -- base case is 0
    body n = Apply (Variable "f") (body (n - 1)) -- recursive case apply f to shrinking i

-------------------------

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x == y = x : merge xs ys
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

------------------------- Assignment 2

-- No input executed with take or !!
variables :: [Var]
-- Infinite list of variable names: a..z, then a1..z1, then a2..z2, etc
-- s is the suffix: "" first, then "1", "2", "3", ... (infinite)
-- For each suffix, c runs through every letter 'a'..'z',
-- and [c] ++ s makes the name (e.g. "b" ++ "1" = "b1").
variables = [[c] ++ s | s <- "" : map show [1 ..], c <- ['a' .. 'z']]

filterVariables :: [Var] -> [Var] -> [Var]
filterVariables xs ys = [x | x <- xs, not (elem x ys)]

fresh :: [Var] -> Var
fresh = undefined

used :: Term -> [Var]
used = undefined

------------------------- Assignment 3

rename :: Var -> Var -> Term -> Term
rename x y (Variable z) = undefined
rename x y (Lambda z n) = undefined
rename x y (Apply n m) = undefined

substitute :: Var -> Term -> Term -> Term
substitute = undefined

------------------------- Assignment 4

beta :: Term -> [Term]
beta = undefined

normalize :: Term -> [Term]
normalize = undefined

normal :: Term -> Term
normal = undefined

-------------------------

a_beta :: Term -> [Term]
a_beta = undefined

a_normalize :: Term -> [Term]
a_normalize = undefined

a_normal :: Term -> Term
a_normal = undefined

-------------------------

example1 :: Term
example1 = undefined

example2 :: Term
example2 = undefined