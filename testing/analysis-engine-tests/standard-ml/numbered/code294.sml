(* old test case name: code294.sml *)

(* EXAMPLE1 - Untypable *)
datatype t = f
structure S = struct val f = () end

(* EXAMPLE2 - Untypable *)
datatype t = f
structure S = struct val f = () end
val _ = S.f + 1;
