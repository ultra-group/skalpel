(* old test case name: code404.sml *)

(* Untypable *)
val stuff = 5 + true

fun sum (h::t) = sum t + h
  | sum [] = 0;
