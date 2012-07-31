(* old test case name: code145.sml *)

datatype t = c of int
val c as (u, v) = (1, 2)
val _ = c 1
