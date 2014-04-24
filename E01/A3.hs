module E01.A3 where

-- length und maximum sind bereits in Prelude definiert.
-- In der Klausur stehen die Funktionen aus Prelude üblicherweise leider
-- nicht zur Verfügung, daher definieren wir sie hier erneut, müssen aber
-- den automatischen Import mit hiding unterdrücken.
import Prelude hiding (length, maximum)

-- (a)
l :: [[Int]]
l = [[1,2],[],[-1]] -- Ergebnis: max_length l = 2

-- (b)
max_length :: [[Int]] -> Int
max_length ls = maximum (lengths ls)
-- Klammern können mit maximum $ lengths ls vermieden werden

-- Für Experten: list comprehension
max_length1 :: [[Int]] -> Int
max_length1 ls = maximum [length l | l <- ls]

-- Für Experten: map
-- Achtung: Die Funktion map ist in der Klausur üblicherweise nicht
-- vordefiniert.
max_length2 :: [[Int]] -> Int
max_length2 ls = maximum (map length ls)

-- Klammern vermeiden mit $-Operator
max_length3 :: [[Int]] -> Int
max_length3 ls = maximum $ map length ls

lengths :: [[Int]] -> [Int]
lengths [] = []
-- Zum Verständnis: x :: [Int], xs :: [[Int]]
lengths (x:xs) = length x:lengths xs

length :: [Int] -> Int
length [] = 0
length (x:xs) = 1 + length xs

-- Für Experten: Mittels Faltung und anonymer Funktion (Lambda Term \).
-- Achtung: Die Funktion foldr ist in der Klausur üblicherweise nicht
-- vordefiniert.
length1 :: [Int] -> Int
length1 = foldr (\x y -> y + 1) 0

maximum :: [Int] -> Int
maximum [] = error "A3.maximum: empty list"
maximum [x] = x
maximum (x:xs) = if x > maximum xs then x else maximum xs

-- Alternativ: doppeltes maximum xs mit let-expression vermeiden
maximum1 :: [Int] -> Int
maximum1 [] = error "A3.maximum1: empty list"
maximum1 [x] = x
maximum1 (x:xs) = let m = maximum1 xs in if x > m then x else m

-- Alternativ: doppeltes maximum xs mit where vermeiden
maximum2 :: [Int] -> Int
maximum2 [] = error "A3.maximum2: empty list"
maximum2 [x] = x
maximum2 (x:xs) = if x > m then x else m
  where
    m = maximum2 xs

-- Siehe let vs. where:
-- http://www.haskell.org/haskellwiki/Let_vs._Where

