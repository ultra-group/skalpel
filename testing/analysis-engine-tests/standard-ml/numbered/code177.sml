(* old test case name: code177.sml *)

val v = 1
val w = 1
val u = {x = 1, y = 2, z = 3}
val c as {x, y, z} = {x = u, y = v, z = w}
val b = c = x;
