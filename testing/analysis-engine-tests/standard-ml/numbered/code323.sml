(* old test case name: code323.sml *)

(* Untypable *)
signature s = sig
    structure V : sig
        type t
    end
    val f : V.t
end
structure S : s = _structS
structure T = S.X

(* Similarly: *)
signature s = sig
   structure U : sig
       structure V : sig
           type t
       end
       val f : V.t
   end
end
structure S : s = _structS
structure T = S.X
