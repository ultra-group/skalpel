(* old test case name: code322.sml *)

(* Untypable. *)
signature s =
  sig
    structure T : sig
	eqtype t
        val f : unit
    end
  end

structure S : s = _structS

(* S has signature s but X is not specified in the signature s.*)
structure P = S.X;
