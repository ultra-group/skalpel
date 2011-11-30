(* Untypable - The example differs from the typable testcase
 * 488 by the type of x (from X.t to X.u). *)
signature s = sig
    structure X : sig
        type t
	type u
        val f : t -> unit
    end
    val x : X.u
end
structure S :> s = _structS
val _ = S.X.f S.x
