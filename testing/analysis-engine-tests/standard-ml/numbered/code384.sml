(* old test case name: code384.sml *)

(* untypable *)
datatype t = g
structure S = struct val g = fn () => [] end
val f = S.g ();
