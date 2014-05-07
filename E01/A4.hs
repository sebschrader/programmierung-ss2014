module E01.A4 where

-- (a)
-- Liste aller positiven ganzen Zahlen s = [1,2,3,...]

gen :: [Int]
gen = gen1 1

gen1 :: Int -> [Int]
gen1 a = a:gen1 (a+1)

-- Für Experten: List comprehensions.
gen2 :: [Int]
gen = [1..]

-- (b)
{-
  Haskell nutzt lazy-evaluation oder auch call-by-need genannt, obwohl der
  Begriff call im Kontext von Haskell mit Vorsicht zu genießen ist.
  Dies erlaubt es Kontrukte wie in Aufgabe (a), eine Liste, die nie endet.
  Würde eine call-by-value-Strategie angewendet, könnte ein Aufruf der
  Funktion nie terminieren.
-}

