(* old test case name: code142.sml *)

exception x of int and x of int
val x as (u, v) = (1, 2)
datatype t = c of int | c of int
val c as (u, v) = (1, 2);
