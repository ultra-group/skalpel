(* old test case name: code544.sml *)

(* Untypable *)

infix $$
fun (x $$ y) = x + y
val _ = 1 $$ true;
