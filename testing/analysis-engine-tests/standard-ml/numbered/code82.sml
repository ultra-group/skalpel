(* old test case name: code82.sml *)

(* the fun and datatype do not conflict *)
fun f x = x
datatype t = f;
f 1;
