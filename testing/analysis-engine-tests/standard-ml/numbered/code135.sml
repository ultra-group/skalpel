(* old test case name: code135.sml *)

datatype 'a t = Red of 'a
val (x, x) = (Red 2, 1)
val u = x ()
