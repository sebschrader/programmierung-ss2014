module E02.A2 where

data Lst = Snoc Lst Int | Nil deriving Show

-- (a)
toLst :: [Int] -> Lst
toLst [] = Nil
toLst (x:xs) = Snoc (toLst xs) x

toList :: Lst -> [Int]
toList Nil = []
toList (Snoc xs x) = x:toList xs

-- (b)
{-
  Die Laufzeitkomplexität von Einfügen und Zugriff am Ende einer Liste ist
  O(n), wobei n die Länge der Liste ist, am Kopf der Liste hingegen O(1).
  Dies ist natürlich eher ungünstig.
-}

-- (c)
-- Idee: Warteschlange durch zwei Listen darstellen; erste Liste ist der
-- Anfang der Warteschlange von links gelesen; zweite Liste ist das Ende der
-- Warteschlange von rechts gelesen. Beim Einfügen muss gegebenfalls neu
-- balanciert werden.
type Queue = ([Int], [Int])

isEmpty :: Queue -> Bool
isEmpty ([], _) = True
isEmpty _ = False

enqueue :: Queue -> Int -> Queue
enqueue (xs, ys) y = mkProper (xs, y:ys)

first :: Queue -> Int
first ([], _) = error "E2.A2.first: empty queue"
first (x:_, _) = x

rest :: Queue -> Queue
rest ([], _) = error "E2.A2.rest: empty queue"
rest (x:xs, ys) = mkProper (xs, ys)

mkProper :: Queue -> Queue
mkProper ([], ys) = (reverse ys, [])
mkProper q = q


