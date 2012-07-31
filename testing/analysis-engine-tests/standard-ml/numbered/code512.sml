(* old test case name: code512.sml *)

(* Untypable *)
structure X = struct
structure U = struct type t = int val x = true end
structure V = struct type u = int end
end

signature s = sig
    structure U : sig type t val x : t end
    structure V : sig type u end
    sharing type V.u = U.t
end

structure S :> s = X
