(* old test case name: code134.sml *)

fun f x = x : string
  | f x = x
and f x = x : bool
val ex = f 1;
