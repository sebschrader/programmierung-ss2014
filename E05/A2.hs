module E05.A2 where

data Tree a = Node a [Tree a] deriving Show

class Zippable z where
  genericZipWith :: (a -> b -> c) -> z a -> z b -> z c
  genericZip :: z a -> z b -> z (a, b)
  -- Hinweis: Der Operator (,) :: a -> b -> (a, b) erzeugt aus zwei Werten
  -- beliebigen Typs ein Tupel
  genericZip = genericZipWith (,)

-- Die Funktionen zip und zipWith für Listen existieren ja bereits, also ist die
-- die Definition für den Listentyp [] trivial.
instance Zippable [] where
  genericZipWith = zipWith
  genericZip = zip

-- Für den Tree wird es nun schon etwas haariger. 
instance Zippable Tree where
  -- Bedeutung der Variablennames:
  -- - f function
  -- - lv, rv: left bzw. right value
  -- - lcs, rcs: left bzw. right children
  genericZipWith f (Node lv lcs) (Node rv rcs)
    = Node (f lv rv) (zipWith (genericZipWith f) lcs rcs)

