(* old test case name: code224.sml *)

val _ = fn (x : int) => (x : bool)

datatype 'a T = c of 'a
val (c 1, c true) = (c 1, c true)
