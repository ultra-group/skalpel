(* old test case name: code445.sml *)

(* Untypable *)
fun f () = ()
val _ = fn (f x, f) => x

datatype t = f of int
val (f x) = f;
