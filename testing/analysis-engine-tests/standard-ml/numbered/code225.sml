(* old test case name: code225.sml *)

datatype T = c of int | d of bool
val (c x, d x) = (c 1, d true)
val _ = x;
