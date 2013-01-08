(* old test case name: code401.sml *)

(* Untypable *)
structure S = struct val f = fn x => x + 1 end
open S
overload f : 'a -> int with 'a in (bool)
val _ = f 1;
