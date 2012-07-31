(* old test case name: code223.sml *)

(* we can find a way to have the following piece of code typable *)
(* the most obvious one is to have y = C and overload + *)
datatype t = C
fun f x = (x C, x y)
val _ = y + 1
