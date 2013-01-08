(* old test case name: code186.sml *)

val f = v1 v2
val h = let val g = f in (1 :: g, true :: g) end;
