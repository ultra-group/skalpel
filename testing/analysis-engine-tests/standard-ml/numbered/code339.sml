(* old test case name: code339.sml *)

(* Untypable *)
fun op + x : int = x
val _ = fn x => fn y => x + y
