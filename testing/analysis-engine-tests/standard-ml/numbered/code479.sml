(* old test case name: code479.sml *)

(* Untypable - Value polymorphism restriction through a structure *)
structure S1 = struct datatype t = g1 end
val x = S1.g1
fun f1 () = ()
structure S1 = struct val g1 = fn () => [] end
val f1 = S1.g1 ();
val _ = (x :: f1, true :: f1);
