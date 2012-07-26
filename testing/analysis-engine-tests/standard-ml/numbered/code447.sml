(* Untypable *)
fun f () = ()
val _ = fn f => fn f => (f 1, f true)
