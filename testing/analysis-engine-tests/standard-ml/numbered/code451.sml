(* old test case name: code451.sml *)

(* Untypable - Multi opening of structures *)
structure S = struct structure T = struct val x = () end end
structure T = struct val x = true end
open S T
val _ = x + 1;
