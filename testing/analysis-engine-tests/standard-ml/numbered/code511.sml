(* old test case name: code511.sml *)

(* Untypable *)
signature s = sig type t type u val x : t sharing type t = u end
structure S :> s = struct type t = int type u = int val x = true end;
