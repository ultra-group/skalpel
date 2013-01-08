(* old test case name: code425.sml *)

(* Untypable *)
structure S = struct
  structure T = struct val x = true end
  open T
  structure T = struct val x = () end
end
open S
val _ = x + 1;
