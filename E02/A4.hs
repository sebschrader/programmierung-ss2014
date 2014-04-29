module E02.A4 where

data Expr = Lit Int
          | Var Char
          | Add Expr Expr
          | Mul Expr Expr
          | Exp Expr Int
          deriving Eq -- Für Experten: Instanzen von Eq implementieren ==
--          deriving Show

-- Mit type kann man eine, bestehenden Typ einen neuen Namen geben.
type Assignment = Char -> Int

eval :: Expr -> Assignment -> Int
eval (Lit n) _ = n
eval (Var c) a = a c
eval (Add e1 e2) a = eval e1 a + eval e2 a
eval (Mul e1 e2) a = eval e1 a * eval e2 a
eval (Exp e n) a = eval e a ^ n

-- Beispiel für einen Wert vom Typ expr:
-- 3x^2 + 2
e :: Expr
e = Add (Mul (Lit 3) (Exp (Var 'x') 2))
        (Lit 2)

-- Beispiel für eine Variablenbelegung
a :: Assignment
a 'x' = 4
a  _  = 0

-- (b)
display :: Expr -> String
display (Lit n) = show n
display (Var c) = [c]
display (Add e1 e2) = "(" ++ display e1 ++ " + " ++ display e2 ++ ")"
display (Mul e1 e2) = "(" ++ display e1 ++ " * " ++ display e2 ++ ")"
display (Exp e n)   = display e ++ "^" ++ show n

-- Für Experten: Wir definieren Expr als Instanz der Typklasse Show und müssen
-- die Funktion show für Expr definieren. (Kommt vielleicht später in der
-- Vorlesung)
instance Show Expr where
  show = display

-- (c)
diff :: Expr -> Char -> Expr
diff (Lit _) _ = Lit 0
diff (Var c) d | c == d = Lit 1
               | otherwise = Lit 0
diff (Add e1 e2) d = Add (diff e1 d) (diff e2 d)
diff (Mul e1 e2) d = Add (Mul e1 (diff e2 d)) (Mul (diff e1 d) e2)
diff (Exp e n) d = Mul (Mul (Lit n) (Exp e (n-1))) (diff e d)

-- Zusatz: Um die Terme, die diff erzeugt ein wenig aufzuhübschen.
simplifyStep :: Expr -> Expr
simplifyStep e@(Lit _) = e
simplifyStep e@(Var _) = e
simplifyStep (Add (Lit 0) e2) = simplifyStep e2
simplifyStep (Add e1 (Lit 0)) = simplifyStep e1
simplifyStep (Add (Lit n1) (Lit n2)) = Lit (n1 + n2)
simplifyStep (Add e1 e2) = Add (simplifyStep e1) (simplifyStep e2)
simplifyStep (Mul (Lit 0) e2) = Lit 0
simplifyStep (Mul e1 (Lit 0)) = Lit 0
simplifyStep (Mul (Lit 1) e2) = simplifyStep e2
simplifyStep (Mul e1 (Lit 1)) = simplifyStep e1
simplifyStep (Mul (Lit n1) (Lit n2)) = Lit (n1 * n2)
simplifyStep (Mul e1 e2) = Mul (simplifyStep e1) (simplifyStep e2)
simplifyStep (Exp e 0) = Lit 1
simplifyStep (Exp e 1) = simplifyStep e
simplifyStep (Exp e n) = Exp (simplifyStep e) n

-- Unter Umständen muss simplifyStep mehrfach ausgeführt werden, bis keine
-- Vereinfachungen mehr möglich ist, daher vereinfachen wir solange bis die
-- Vereinfachung nichts mehr ändert.
isSimple :: Expr -> Bool
isSimple e = e == simplifyStep e

-- Hinweis: until ist Teil der Standardbibliothek.
simplify = until isSimple simplifyStep

