(* old test case name: code383.sml *)

(* untypable *)
structure S = struct val g = () end
val f = S.g ()
val _ = (1 :: f, true :: f);
