(* Untypable - sharing. *)
signature s = sig type t type u sharing type t = u end
structure S :> s = struct type t = bool type u = int end
