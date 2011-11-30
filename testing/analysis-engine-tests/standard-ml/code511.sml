(* Untypable *)
signature s = sig type t type u val x : t sharing type t = u end
structure S :> s = struct type t = int type u = int val x = true end
