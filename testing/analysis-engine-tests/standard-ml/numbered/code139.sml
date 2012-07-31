(* old test case name: code139.sml *)

fun g y z = if z then 1 + y else y
fun f y = g y y
