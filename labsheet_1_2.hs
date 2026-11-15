--Lists

halving :: Int -> [Int]
halving n | n == 0 = []
          | odd n  = n : halving (n-1)
          | even n = n : halving (div n 2)

collatz :: Int -> [Int]
collatz n 
    | n == 0 = []
    | n == 1 = [1]
    | even n = n : collatz (div n 2)
    | odd n = n : collatz (n*3+1)

colLength :: Int -> Int
colLength n = length (collatz n)

-- Pattern Matching

maxList :: [Int] -> Int
maxList [] = 0
maxList [x] = x
maxList (x:xs) = max x (maxList xs)


allDucks :: [String] -> Bool
allDucks [] = True
allDucks (x:xs) = (x == "duck") && allDucks xs

-- Alternative to above answer for future reference
-- allDucks :: [String] -> Bool
-- allDucks []          = True          -- 1. Matches ONLY the empty list []
-- allDucks ("duck":xs) = allDucks xs   -- 2. Matches a non-empty list whose head is "duck"
-- allDucks (_:_)       = False         -- 3. Matches ANY OTHER non-empty list (e.g. ["goose", "duck"])

duckDuckGoose :: [String] -> Bool
duckDuckGoose ("duck":xs) = duckDuckGoose xs
duckDuckGoose ("goose":[]) = True
duckDuckGoose ("goose":xs) = False
duckDuckGoose _ = False

--Pairs

ducks :: [(String,Int)]
ducks = [("Donald",6),("Daisy",5),("Huey",2),("Louie",2),("Dewey",2)]

noDDucks :: [(String,Int)] -> [String]
noDDucks [] = []
noDDucks ((name,age):xs)
  | head name == 'D' = noDDucks xs
  | otherwise        = (name:_) : noDDucks xs

youngOrShort :: [(String,Int)] -> Bool
youngOrShort [] = False
youngOrShort ((name,age):xs)
    | length name <= 3 || age < 3 = True
    | otherwise                   = youngOrShort xs

describeDucks :: [(String,Int)] -> String
describeDucks [] = ""
describeDucks ((name,age):xs) = name ++ " is a duck who is " ++ show age ++ " years old. " ++ describeDucks xs