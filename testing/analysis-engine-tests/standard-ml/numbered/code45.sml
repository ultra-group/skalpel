(* old test case name: code45.sml *)

datatype t = f of int
val rec f = fn () => 1
val f x = f 2
