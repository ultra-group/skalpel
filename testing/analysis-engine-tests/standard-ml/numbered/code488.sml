(* old test case name: code488.sml *)

(* Typable *)
signature s = sig
    structure X : sig
        type t
	type u
        val f : t -> unit
    end
    val x : X.t
end
structure S :> s = _structS
val _ = S.X.f S.x
