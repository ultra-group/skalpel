(* old test case name: code183.sml *)

val f = (fn x => x) []
val _ = let val g = (fn h => h :: f) in (g 1, g true) end
