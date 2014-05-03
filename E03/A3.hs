module E03.A3 where

-- (a)
nonnegatives :: [Int] -> [Int]
nonnegatives [] = []
nonnegatives (x:xs) = if x < 0 then nonnegatives xs
                               else x:nonnegatives xs
-- Für Experten: Implemtierung mit Higher Order Function filter, die uns eine
-- Liste mit allen Elementen einer Ausgangsliste liefiert, die ein Prädikat
-- (Funktion, die auf Bool abbildet) erfüllen.
-- filter :: (a -> Bool) -> [a] -> [a]
nonnegatives1 :: [Int] -> [Int]
nonnegatives1 l = filter (>= 0) l

-- Noch besser: sog. point-free style
nonnegatives2 :: [Int] -> [Int]
nonnegatives2 = filter (>= 0)

-- (b)
count :: [a] -> Int
count [] = 0
count (x:xs) = 1 + count xs

-- Für Experten: Kleine Optimierung, um die Erstellung von zu vielen Thunks zu
-- vermeiden.
-- Siehe http://www.haskell.org/haskellwiki/Thunk
count1 :: [a] -> Int
count1 xs = countAcc xs 0
  where
    countAcc [] n = n
    countAcc (_:xs) n = countAcc xs (n+1)

-- (c)
check :: [Int] -> Bool
check a | count (nonnegatives a) >= 5 = True
        | otherwise = False

check1 :: [Int] -> Bool
check1 a = (count $ nonnegatives a) >= 5

