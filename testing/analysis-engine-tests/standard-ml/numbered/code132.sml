(* old test case name: code132.sml *)

(* Syntacticly incorrect *)
fun f x = x
and f x = x
val (g, g x) = (1, 2);
