(* old test case name: code141.sml *)

datatype 'a t = y of 'a
val f = x (y (z a))
val _ = f f
