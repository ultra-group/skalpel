(* Untypable *)
val x = true
structure S = struct val x = true end
open S
val x = true
val _ = x : int
