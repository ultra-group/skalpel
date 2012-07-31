(* old test case name: code159.sml *)

(* In this example the type of y cannot be generalised at the inner val
   bacause it contains 'a which is also in the context and the generalisation
   is not over the type variable of the context *)
fun f x = let val y = x : 'a in () end
val _ = f 1
