(* old test case name: code447.sml *)

(* Untypable *)
fun f () = ()
val _ = fn f => fn f => (f 1, f true)
