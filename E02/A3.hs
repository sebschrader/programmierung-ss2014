module E02.A3 where

data Tree = Leaf Int | Branch Tree Tree deriving Show

-- (a)
t :: Tree
t = Branch (Branch (Leaf 1) (Leaf 2))
           (Branch (Leaf 3) (Branch (Leaf 4) (Leaf 5)))

-- (b)
countLeafs :: Tree -> Int
countLeafs (Leaf _)     = 1
countLeafs (Branch l r) = countLeafs l + countLeafs r

toList :: Tree -> [Int]
toList (Leaf n) = [n]
toList (Branch l r) = toList l ++ toList r


