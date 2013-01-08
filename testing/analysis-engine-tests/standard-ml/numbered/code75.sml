(* old test case name: code75.sml *)

(* multi-occurences errors *)
val (f, f) = (true, false)
val ex1 = f + 1
fun g x = true
  | h x = 1
val ex2 = (fn (x, x) => (g ()) + x) (false, true)
type w = int and w = bool and w = string
datatype ('a, 'b) t = C | C of w and 'a t = D of t
val (C x) = C 3
exception e and e of int
val ex3 = e true;
