(* old test case name: code190.sml *)

datatype t = C of t -> int
val x = fn (_ : t) => 7
val z = let val u = x (C x) in () end
