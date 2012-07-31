(* old test case name: code412.sml *)

structure S = struct val x = true end
structure T = struct val x = 1 end
open T S
(* Untypable *)
val _ = x + 1
open S T
(* Typable *)
val _ = x - 1
