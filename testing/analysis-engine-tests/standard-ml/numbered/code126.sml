(* old test case name: code126.sml *)

datatype ('a, 'b, 'c) t = Red    of 'a * 'b * 'c
			| Blue   of 'a * 'b * 'c
			| Pink   of 'a * 'b * 'c
                        | Green  of 'a * 'b * 'c
			| Yellow of 'a * 'b * 'c
			| Orange of 'a * 'b * 'c
datatype ('a, 'b) u = SpeCol of ('a, 'a, 'b) t * 'b
		    | UniCol of ('a, 'a, 'a) t * 'b
fun trans (Red    (x, y, z)) = Blue   (y, x, z)
  | trans (Blue   (x, y, z)) = Pink   (y, x, z)
  | trans (Pink   (x, y, z)) = Green  (y, x, z)
  | trans (Green  (x, y, z)) = Yellow (y, x, z)
  | trans (Yellow (x, y, z)) = Orange (y, x, z)
  | trans (Orange (x, y, z)) = Red    (y, x, z)
and touni (Red    (x, _, _)) = Red    (x, x, x)
  | touni (Blue   (x, _, _)) = Blue   (x, x, x)
  | touni (Pink   (x, _, _)) = Pink   (x, x, x)
  | touni (Green  (x, _, _)) = Green  (x, x, x)
  | touni (Yellow (x, _, _)) = Yellow (x, x, x)
  | touni (Orange (x, _, _)) = Orange (x, x, x)
and getLast (Red    (_, _, z)) = z
  | getLast (Blue   (_, _, z)) = z
  | getLast (Pink   (_, _, z)) = z
  | getLast (Green  (_, _, z)) = z
  | getLast (Yellow (_, _, z)) = z
  | getLast (Orange (_, _, z)) = z
fun pretrans (SpeCol (col, b)) =
    if b = getLast col
    then SpeCol (trans col, b)
    else UniCol (touni col, b)
  | pretrans x = x
val x = SpeCol (Red (2, 2, false), true)
val y = pretrans x;
fun f u = (pretrans x, pretrans y,
	   (trans o
	    trans o trans o trans o trans o trans o
	    trans o trans o trans o trans o trans o
	    trans o trans o trans o trans o trans o
	    trans o trans o trans o trans o trans o
	    trans) u);
