(* untypable *)
structure T = struct val y = false end
open T
structure S = struct val x = true end
open S
val _ = x + y
