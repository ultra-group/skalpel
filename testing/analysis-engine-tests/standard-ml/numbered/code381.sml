(* old test case name: code381.sml *)

(* untypable *)
datatype t = g of unit
fun g () = []
val f = g ();
val _ = (1 :: f, true :: f);
