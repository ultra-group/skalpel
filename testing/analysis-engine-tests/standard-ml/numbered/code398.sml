(* old test case name: code398.sml *)

(* Untypable *)
structure S = struct val x = true end
open S
val _ = x + 1
