(* Untypable under context dependency. *)
fun f1 () = ()
structure S1 = struct val g1 = fn () => [] end
val f1 = S1.g1 ();
val _ = (1 :: f1, true :: f1)

(* Similarly: *)
fun f2 () = ()
val g2 = fn () => []
val f2 = g2 ();
val _ = (1 :: f2, true :: f2)

(* Untypable and g3 is not a context dependency. *)
fun g3 () = ()
val g3 = fn () => []
val f3 = g3 ();
val _ = (1 :: f3, true :: f3)
