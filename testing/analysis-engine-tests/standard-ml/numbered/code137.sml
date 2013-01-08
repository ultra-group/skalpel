(* old test case name: code137.sml *)

(* one can try to replace the second occurrence of t by int *)
datatype t = C | D of t
fun f x = D x
val ex = f true;
