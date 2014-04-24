module A2 where

f :: [Int] -> [Int]
f [] = []
f (x:xs) = if x > 0 then f xs ++ [x] else f xs

-- Für Experten: filter und reverse, partielle Applikation (> 0),
-- Applikationsoperator $ zur Klammervermeidung
-- Achtung: filter und reverse sind nicht in der Klausur vordefiniert.
f1 :: [Int] -> [Int]
f1 xs = reverse $ filter (> 0) xs

-- Quizfrage: Vergleiche die Effizienz beider Lösungen.
-- Hinweis: Die Standardimplementierung von reverse und filter haben die
-- Laufzeitkomplexität von O(n)

