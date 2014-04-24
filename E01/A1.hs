--Deklariere das Modul A1
module A1 where

-- (a)
square :: [Int] -> [Int]
square [] = []
-- x: Int, xs :: [Int]
square (x:xs) = x * x:square xs

-- Alternativ mit Potenzoperator (^)
square1 :: [Int] -> [Int]
square1 [] = []
square1 (x:xs) = x^2:square1 xs

-- Alternativ mit Listenkonkatenation (++) (unüblich)
square2 :: [Int] -> [Int]
square2 [] = []
square2 (x:xs) = [x ^ 2] ++ square2 xs

-- Für Experten: map und partielle Applikation (^2)
-- Achtung: Die Funktion map ist in der Klausur üblicherweise nicht
-- vordefiniert.
square3 :: [Int] -> [Int]
square3 xs = map (^2) xs

-- square3 noch kürzer: sog. point-free style
square4 :: [Int] -> [Int]
square4 = map (^2)
-- map :: (a -> b) -> [a] -> [b] und (^2) :: Num a => a -> a, daher
-- map (^2) :: Num a => [a] -> [a]

-- (b)
at :: [Int] -> Int -> Int
at [] _ = error "A1.at: empty list"
at (x:_)  0 = x
at (_:xs) n = at xs (n-1)

-- Alternativ mit if-then-else statt pattern matching (nicht so schön)
at1 :: [Int] -> Int -> Int
at1 [] _ = error "A1.at1: empty list"
at1 (x:xs) n = if n == 0 then x else at1 xs (n-1)

-- (c)
-- Mit Guards:
dup :: [Int] -> [Int] -> [Int]
dup [] _ = []
dup _ [] = []
dup (x:xs) ys | occurs x ys = x:dup xs ys
              | otherwise   = dup xs ys

-- Alternativ mit if-then-else (nicht so schön):
dup1 :: [Int] -> [Int] -> [Int]
dup1 [] _ = []
dup1 _ [] = []
dup1 (x:xs) ys = if occurs x ys then x:dup1 xs ys else dup1 xs ys

--Für Experten: Implementierung von äquivalenter Funktion intersect in
--der Standardbibliothek:
--http://hackage.haskell.org/package/base-4.7.0.0/docs/src/Data-List.html#intersect

-- Direkt als logischer Ausdruck:
occurs :: Int -> [Int] -> Bool
occurs _ [] = False
occurs x (y:ys) = x == y || occurs x ys

-- Alternativ mit Guards:
occurs1 :: Int -> [Int] -> Bool
occurs1 _ [] = False
occurs1 x (y:ys) | x == y = True
                | otherwise = occurs1 x ys

-- Alternativ mit if-then-else:
occurs2 :: Int -> [Int] -> Bool
occurs2 _ [] = False
occurs2 x (y:ys) = if x == y then True else occurs2 x ys

--Für Experten: Implementierung von äquivalenter Funktion elem in
--Standardbibliothek
--http://hackage.haskell.org/package/base-4.7.0.0/docs/src/GHC-List.html#elem

