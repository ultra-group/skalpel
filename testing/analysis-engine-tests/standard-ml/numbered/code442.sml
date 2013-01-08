(* old test case name: code442.sml *)

(* Untypable *)
structure S = struct
  fun f () = ()
  val f = true
end
open S
val _ = fn f => (f true, f 1);
