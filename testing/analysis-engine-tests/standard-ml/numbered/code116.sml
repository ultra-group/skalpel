(* old test case name: code116.sml *)

val x = let datatype t = C of 'a t | D in C D end
val y = x
