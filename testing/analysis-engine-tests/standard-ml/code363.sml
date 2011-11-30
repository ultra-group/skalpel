(* Untypable *)
structure S = struct fun g () = [] end
val f = S.g ();
val _ = (1 :: f, true :: f)
