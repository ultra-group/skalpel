(* old test case name: code372.sml *)

(* untypable *)
val f = let val x = [] in x end
structure S = struct val f = f end
val _ = (1 :: S.f, true :: S.f)
