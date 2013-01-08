(* old test case name: code92.sml *)

datatype u = U
datatype 'a u = C of int u | D
val x = (C D, C);
