(* old test case name: code428.sml *)

(* Untypable *)
fun f () = ()
structure S = struct val f = () end
open S
val _ = (fn f => f ()) ()
