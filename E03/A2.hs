module E03.A2 where

-- Wir verstecken das vordefinierte map, um es später erneut zu definieren.
import Prelude hiding (map)

data Tree a = Leaf a | Branch a (Tree a) (Tree a) deriving Show

-- (a)
liste :: Tree a -> [a]
liste (Leaf x) = [x]
liste (Branch x l r) = liste l ++ [x] ++ liste r

-- (b)
-- Zur Erinnerung: map ist wie folgt definiert
map :: (a -> b) -> [a] -> [b]
map _ [] = []
-- Die Typen sind wie folgt x :: a , xs :: [a]
map f (x:xs) = f x:map f xs
-- map erlaubt es uns also, jedes Element einer Liste mit einer vorgegeben
-- Funktion zu transformieren. Die Liste an sich bleibt erhalten.
-- Beispiel map (+1) [1,2,3,4] = [2,3,4,5]
-- Hinweis: (+1) ist partielle Anwendung des Arguments 1 auf die Funktion (+),
-- der Additionsoperator, also eine Funktion, die 1 addiert.

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Branch x l r) = Branch (f x) (mapTree f l) (mapTree f r)

-- Für Experten: Container-Datentypen implementieren üblicherweise die Klasse
-- Functor, die das Konzept von map verallgemeinert.
instance Functor Tree where
  fmap = mapTree

