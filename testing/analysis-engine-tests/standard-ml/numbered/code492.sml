(* old test case name: code492.sml *)

(* Untypable - The example differs from the untypable testcase
 * 489 by the use of functors. *)
signature s = sig
    structure X : sig
        type t
	type u
        val f : t -> unit
    end
    val x : X.u
end

functor F (S : s) = S :> s

functor G (S : s) = struct
structure T = F(S)
val _ = T.X.f T.x
end
