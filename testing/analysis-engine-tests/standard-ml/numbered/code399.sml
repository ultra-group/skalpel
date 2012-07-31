(* old test case name: code399.sml *)

(* Untypable *)
val y = 1
signature s = sig type t = bool val x : t end
val z = y
structure S :> s = struct type t = bool val x = true end
val x = z
open S
val _ = x + 1
