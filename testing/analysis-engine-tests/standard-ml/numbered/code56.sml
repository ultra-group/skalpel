(* old test case name: code56.sml *)

(* sml/nj doesn't complain about that but Hamlet does *)
datatype t = f
fun f f f = ();
(* sml/nj complains about *)
datatype t = f
val rec f = fn (f, f) => ();
