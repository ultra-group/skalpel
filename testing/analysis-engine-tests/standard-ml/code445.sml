(* Untypable *)
fun f () = ()
val _ = fn (f x, f) => x

datatype t = f of int
val (f x) = f
