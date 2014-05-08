module E04.A3 where

-- Die Klasse Num ist schon im Prelude vordefiniert, sie einfach per hiding zu
-- verstecken würde uns aber nur noch mehr Probleme verschaffen, da dann auch
-- die entsprechenden Definitionen der Funktionen von Num auf Int und Float
-- verloren gehen. Es gäbe Wege, dass zu umgehen, aber der Einfachheit halber
-- verwende ich einfach die Definition von Num aus dem Prelude.

-- Eigentlich könnte man den einzigen Typkonstruktor CmplxNumber von Cmplx auch
-- Cmplx, also genauso wie den Typ benennen, da die Namensräume von Typen und
-- Typkonstrukturen disjunkt sind.
-- Dies ist eigentlich auch zu bevorzugen, um Verwirrung bei Neulingen zu
-- vermeiden, habe ich das hier nicht getan.
data Cmplx = CmplxNumber Float Float deriving Show

instance Num Cmplx where
  -- Hinweis: Anstatt Operatoren infix zu notieren, ist es auch möglich sie
  -- wie folgt als übliche Applikation präfix zu notieren:
  -- (+) (CmplxNumber r1 i1) (CmplxNumber r2 i2) = ...
  (CmplxNumber r1 i1) + (CmplxNumber r2 i2) = CmplxNumber (r1 + r2) (i1 + i2)

  -- (-) muss nicht explizit definiert werden, da schon eine
  -- Standardimplementierung vorhanden ist.
  --(CmplxNumber r1 i1) - (CmplxNumber r2 i2) = CmplxNumber (r1 - r2) (i1 - i2)

  (CmplxNumber r1 i1) * (CmplxNumber r2 i2) = CmplxNumber (r1*r2 - i1*i2)
                                                          (r1*i2 + r2*i1)

  negate (CmplxNumber r i) = CmplxNumber (negate r) (negate i)
  -- Hinweis 0 ist eigentlich vom Typ Integer, bei Literalen fügt der
  -- Compiler die Umwandlung nach Float mittels fromInteger 0 automatisch
  -- ein, bei Variablen findet dies nicht statt.

  fromInteger n = CmplxNumber (fromInteger n) 0

  -- Hinweis: Die Typsignatur von abs verlangt, dass wir wieder eine komplexe
  -- Zahl liefern und keine reelle Zahl, obwohl der Betrag einer komplexen
  -- Zahl immer rein reell ist. Dies ist aber lediglich eine Besonderheit von
  -- komplexen Zahlen. Man betrachte zum Vergleich bspw. den Betrag von
  -- reellen und ganzen Zahlen.
  abs (CmplxNumber r i) = CmplxNumber (sqrt (r^2 + i^2)) 0

  -- Im Gegensatz zum Num aus der Aufgabenstellung ist Num in Prelude noch mit
  -- einer Funktion signum ausgestattet, die uns das Vorzeichen einer Zahl
  -- liefert. Um den Compiler also glücklich zu machen, damit er keine
  -- Warnungen erzeugt, definieren ich diese Funktion eben noch, obwohl es in
  -- der Aufgabenstellung nicht verlangt ist.
  -- Die Idee beim signum von komplexen Zahlen ist, dass sich der signum für
  -- reelle Zahl auch so formulieren lässt signum x = x / abs x und wird
  -- einfach auf komplexe Zahlen erweitert. Das Vorzeichen ist damit eine
  -- Koordinate auf dem Einheitskreis.
  signum (CmplxNumber r i) = CmplxNumber (r / magnitude) (i / magnitude)
    where
      magnitude = sqrt (r^2 + i^2)

-- Für Experten: Warum komplexe Zahlen nur über Floats, nicht auch über
-- beliebige andere Typen von Zahlen?
-- Genau dies macht die Standardbibliothek im Modul Data.Complex:
-- http://hackage.haskell.org/package/base-4.7.0.0/docs/src/Data-Complex.html#Complex

