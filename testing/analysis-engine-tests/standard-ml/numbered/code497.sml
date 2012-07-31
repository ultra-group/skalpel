(* old test case name: code497.sml *)

(* Untypable *)
structure X = struct
structure U = struct type 'a t = bool val x = true end
structure V = struct type 'a t = int fun f x = () end
end

signature s = sig
    structure U : sig type 'a t val x : 'a t         end
    structure V : sig type 'a t val f : 'a t -> unit end
    sharing type V.t = U.t
end

structure S :> s = X

val _ = S.V.f S.U.x
