import Data.Char

---Exercise 1

toUpperSt :: String -> String
toUpperSt [] = []
toUpperSt (c:cs) = toUpper c : toUpperSt cs

deleteDigits :: String -> String
deleteDigits [] = []
deleteDigits (c:cs)
    | isDigit c = deleteDigits cs
    | otherwise  = c : deleteDigits cs

leetSpeak :: String -> String
leetSpeak [] = "!"
leetSpeak (c:cs)
    | (c == 'e') = '7' : leetSpeak cs
    | (c == 'o') = '0' : leetSpeak cs
    | (c == 's') = 'z' : leetSpeak cs
    | otherwise = c : leetSpeak cs

--Exercise 2

-- factors2 :: Int -> [Int]
-- factors2 0 = []
-- factors2 n | (n `mod` 2 == 0) =  2 : factors2 (n `div` 2)
--            | otherwise        = [n]

factorsm :: Int -> Int -> [Int]
factorsm _ 0 = []
factorsm m n
    | (n `mod`` m == 0) = m : (n `div` m)
    | otherwise = [n]

factorsFrom :: Int -> Int -> [Int]
factorsFrom _ 0 = []
factorsFrom m n | n >= m           = [n]
                | (n `mod` m == 0) = m : factorsFrom m (n `div` m)
                | otherwise        = factorsm (m+1) n

primeFactors :: Int -> [Int]
primeFactors n = factorsFrom 2 n