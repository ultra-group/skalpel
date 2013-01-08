(* old test case name: code484.sml *)

(* Untypable - u is opaque so it is different from int *)
signature s = sig type u type t val x : t end
structure S :> s where type u = int = _structS
val _ = S.x + 1;
