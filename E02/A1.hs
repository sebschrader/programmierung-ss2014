module E02.A1 where

-- (a)
cmpr :: [Int] -> [Int] -> Bool
cmpr [] [] = True
cmpr [] _  = False
cmpr _ [] = False
cmpr (x:xs) (y:ys) = x == y && cmpr xs ys

-- (b)
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

-- Mit sog. as-pattern kann man sich sparen, die Listen wieder neu
-- zusammenzubauen
merge1 :: [Int] -> [Int] -> [Int]
merge1 [] ys = ys
merge1 xs [] = xs
merge1 allx@(x:xs) ally@(y:ys) | x < y = x:merge1 xs ally
                    | otherwise = y:merge1 allx ys


