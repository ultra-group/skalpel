(* Untypable *)
datatype ('a, 'b, 'c) t = Red    of 'a * 'b * 'c
			| Blue   of 'a * 'b * 'c
			| Pink   of 'a * 'b * 'c
                        | Green  of 'a * 'b * 'b
			| Yellow of 'a * 'b * 'c
			| Orange of 'a * 'b * 'c
fun trans (Red    (x, y, z)) = Blue   (y, x, z)
  | trans (Blue   (x, y, z)) = Pink   (y, x, z)
  | trans (Pink   (x, y, z)) = Green  (y, x, z)
  | trans (Green  (x, y, z)) = Yellow (y, x, z)
  | trans (Yellow (x, y, z)) = Orange (y, x, z)
  | trans (Orange (x, y, z)) = Red    (y, x, z)
type ('a, 'b) u = ('a, 'a, 'b) t * 'b
val x = (Red (2, 2, false), true)
val y : (int, bool) u = (trans (#1 x), #2 x)
