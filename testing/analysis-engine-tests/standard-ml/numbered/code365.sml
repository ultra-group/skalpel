(* old test case name: code365.sml *)

(* Untypable but g1 is polymorphic *)
fun g1 () = []
datatype t = g1 of unit
val f1 = g1 ();
val _ = (1 :: f1, true :: f1)

(* Untypable because g2 is monomorphic *)
fun g2 () = []
val f2 = g2 ();
val _ = (1 :: f2, true :: f2);
