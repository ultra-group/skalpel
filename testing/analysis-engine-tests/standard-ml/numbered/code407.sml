(* old test case name: code407.sml *)

(* Untypable *)
structure T = struct val x = true end
structure S = struct val x = 1 end
open S T
val _ = x + 1
