(* old test case name: code380.sml *)

(* untypable *)
fun f () = ()
val f = ();
fun g () = []
val f = g ()
val _ = (1 :: f, true :: f);
