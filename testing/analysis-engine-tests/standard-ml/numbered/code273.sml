(* old test case name: code273.sml *)

(* untypable *)
signature S1 = sig structure s : sig end end
signature S2 = sig structure s : S1 end
structure s = struct structure s = struct end end : S2
(* or even simpler: *)
structure s : sig
    structure s : sig
	structure s : sig end
    end
end =
struct structure s = struct end end;
