(* old test case name: code363.sml *)

(* Untypable *)
structure S = struct fun g () = [] end
val f = S.g ();
val _ = (1 :: f, true :: f);
