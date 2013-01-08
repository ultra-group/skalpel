(* old test case name: code537.sml *)

(* Untypable *)

infix 4 $$
fun x $$ y = x + y

val _ = 1 $$ true;
