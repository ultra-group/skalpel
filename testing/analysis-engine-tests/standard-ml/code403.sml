(* Untypable.  Type constructor clash unaffected by an opening. *)
val x = true
structure S = struct end
open S
val _ = x + 1
