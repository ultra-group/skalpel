(* old test case name: code146.sml *)

(* One of the f (left hand side of function) is highlighted to make
   the monomorphic typing of f inside its definition explicit *)
fun f 1 = f 2 | f x = f true | f x = y
