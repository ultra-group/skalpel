(* old test case name: code39.sml *)

fun f1 (x : int)  = x
and g1 (x : bool) = f1 x
fun f2 x : int  = x
and g3 x : bool = f2 x;
