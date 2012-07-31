(* old test case name: code235.sml *)

fun f []  : int = 0
  | f [x] : bool = x
  | f (x :: xs) = x + (f xs)
