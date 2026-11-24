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
-- Build the fixed wrapper lamba f containing lambda x
-- and the body depending on the input num
numeral :: Int -> Term
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
-- Infinite list of variable names: a..z, then a1..z1, then a2..z2, etc
-- s is the suffix: "" first, then "1", "2", "3", ... (infinite)
-- For each suffix, c runs through every letter 'a'..'z',
-- and [c] ++ s makes the name (e.g. "b" ++ "1" = "b1").
variables :: [Var]
variables = [[c] ++ s | s <- "" : map show [1 ..], c <- ['a' .. 'z']]

-- Keep every x from xs unless it is in ys
filterVariables :: [Var] -> [Var] -> [Var]
filterVariables xs ys = [x | x <- xs, not (elem x ys)]

-- Returns the first variable name that is not in the list `taken`
-- It filters the endless list `variables` to remove every taken variable
-- then picks the first one that's left
fresh :: [Var] -> Var
fresh taken = (filterVariables variables (taken)) !! 0

-- Finds every variable name inside a Term and returns them as a sorted list
-- with no repeats. A Variable gives just it's name
-- A Lambda gives its name plus the names inside it
-- An Apply gives the names from both of its parts
-- merge helper above joins two lists together in order removing duplicates.
used :: Term -> [Var]
used (Variable x) = [x]
used (Lambda x m) = merge [x] (used m)
used (Apply n m) = merge (used n) (used m)

------------------------- Assignment 3

-- Find and replace the variable x to y throughout a Term
-- Variable: if its name is x, change it to y, otherwise leave it
-- Apply: rename inside both parts
-- Lambda: if its own name is x, leave it unchanged, because that x is a bound
-- variable otherwise keep the name and rename inside the body
rename :: Var -> Var -> Term -> Term
rename x y (Variable z) = if z == x then Variable y else Variable z
rename x y (Lambda z n) = if z == x then Lambda z n else Lambda z (rename x y n)
rename x y (Apply n m) = Apply (rename x y n) (rename x y m)

-- Replaces the name x with the Term n throughout a Term, avoiding variable capture
-- Variable: swap in n if it's x
-- Apply: substitute in both parts
-- Lambda: if its own bound name is x leave it unchanged otherwise rename its
-- bound name to a fresh z first, so n's variables can't be accidentally captured
-- then substitute inside the body
substitute :: Var -> Term -> Term -> Term
substitute x n (Variable y) = if y == x then n else Variable y
substitute x n (Lambda y m) = if y == x then Lambda y m else Lambda z (substitute x n (rename y z m))
  where
    z = fresh (used m ++ used n ++ [x])
substitute x n (Apply m1 m2) = Apply (substitute x n m1) (substitute x n m2)

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