(* untypable *)
structure S = struct val g = () end
val f = S.g ()
val _ = (1 :: f, true :: f)
