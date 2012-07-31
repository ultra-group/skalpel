(* old test case name: code59.sml *)

fun g x y z  = (z x, z y)
fun f x y    = y + 1
  | f true y = y
  | f u v    = g u v
fun h y x z x w = z
