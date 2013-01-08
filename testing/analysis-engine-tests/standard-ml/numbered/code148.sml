(* old test case name: code148.sml *)

val v1 = 1
val v2 = 2
val v3 = 3
fun createrec v1 v2 v3 = {x1 = v1, x2 = v2, x3 = v3}
val {x1 : bool, x2, x3} = createrec v1 v2 v3;
