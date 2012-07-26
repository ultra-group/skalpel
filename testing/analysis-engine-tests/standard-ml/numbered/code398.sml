(* Untypable *)
structure S = struct val x = true end
open S
val _ = x + 1
