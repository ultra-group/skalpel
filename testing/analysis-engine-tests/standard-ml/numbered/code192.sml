(* old test case name: code192.sml *)

val f1 = (fn x => x) []
fun f2 f x = x :: (f x)
val h = let val g = (fn h => h::f1) in (f2 g 1, f2 g true) end;
