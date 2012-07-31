(* old test case name: code140.sml *)

datatype ('a, 'b) t = Red    of 'a * 'b
                    | Green  of 'a * unit (* and 'a instead of unit *)
fun trans (Red   (x, y)) = Green (x, y)
  | trans (Green (x, y)) = Red   (x, y)
type ('a, 'b) u = ('a, 'b) t * 'a
fun f x = (trans (#1 x), #2 x) : (int, bool) u
