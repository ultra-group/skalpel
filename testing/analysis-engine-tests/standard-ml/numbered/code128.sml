(* old test case name: code128.sml *)

datatype 'a t = Red of 'a
fun trans (Red  x) = Red x
fun f u = (trans o trans) u
