module Extra.IsHeap where

data Tree = Branch Int Tree Tree | Nil deriving Show

-- Baum erfüllt Heap-Eigenschaft, wenn alle Kindknoten eine kleinere (oder
-- gleiche) Bewertung haben.
-- Lässt sich umformulieren zu: Direkte Kindknoten eine kleinere (oder gleiche)
-- Bewertung haben und die direkten Kindknoten ihrerseits die Heap-Eigenschaft
-- erfüllen.
isHeap :: Tree -> Bool
isHeap Nil = True
isHeap (Branch n l r) = hasSmallerKey l n && hasSmallerKey r n && isHeap l && isHeap r

hasSmallerKey :: Tree -> Int -> Bool
hasSmallerKey Nil _ = True
hasSmallerKey (Branch n _ _) m = n <= m

-- Kleine Optimierung: Aktuelles Maximum bei der Prüfung der Teilbäume
-- weitergeben, um doppeltes Pattern Matching über den Kindern zu vermeiden.
isHeap2 :: Tree -> Bool
isHeap2 Nil = True
isHeap2 (Branch n l r) = isHeap2' l n && isHeap2' r n

isHeap2' :: Tree -> Int -> Bool
isHeap2' Nil _ = True
isHeap2' (Branch n l r) m = n <= m && isHeap2' l n && isHeap2' r n

-- Variante: Ohne Hilfsfunktion. Auch wieder doppeltes Pattern Matching.
isHeap3 :: Tree -> Bool
isHeap3 Nil                                          = True
isHeap3 (Branch n Nil              Nil)              = True
isHeap3 (Branch n l@(Branch i _ _) Nil)              = i <= n && isHeap3 l
isHeap3 (Branch n Nil              r@(Branch i _ _)) = j <= n && isHeap3 r
isHeap3 (Branch n l@(Branch i _ _) r@(Branch j _ _)) = i <= n && j <= n && isHeap3 l && isHeap3 r

