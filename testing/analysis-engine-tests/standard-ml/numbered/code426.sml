(* old test case name: code426.sml *)

(* Untypable *)
structure S = struct
  structure T = struct structure U = struct val x = true end end
  open T.U
  structure T = struct structure U = struct val x = () end end
end
open S
val _ = x + 1
