(* old test case name: code310.sml *)

(* In SML/NJ: *)
datatype t = T of int | U of int * int | V of int
fun f1 (true | false) = ()
(* Syntacticaly incorrect *)
fun f2 (T x | U (x, y)) = x
  | f2 (V x) = x
  | f2 _ = 0
(* Syntacticaly correct *)
fun f3 (T x | V x) = x
  | f3 (U (x, y)) = x
  | f3 _ = 0;
