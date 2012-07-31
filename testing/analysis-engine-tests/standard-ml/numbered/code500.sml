(* old test case name: code500.sml *)

(* Typable *)
signature s = sig
    structure U : sig type t val x : t         end
    structure V : sig type t val f : t -> unit end
    structure W : sig type u end
    sharing type W.u = W.u
    sharing type U.t = V.t
end

(*structure S :> s = _S*)
functor F (S : s) = struct
val _ = S.V.f S.U.x
end
