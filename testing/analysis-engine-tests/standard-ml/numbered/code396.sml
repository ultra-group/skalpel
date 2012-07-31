(* old test case name: code396.sml *)

(* Untypable *)
datatype t = C of bool
fun f (C x) = x + 1
