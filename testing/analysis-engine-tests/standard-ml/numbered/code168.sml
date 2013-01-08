(* old test case name: code168.sml *)

fun f x = x
fun h x = x
fun g x = if f x then h 1 else h 0
val _ = g 1;
