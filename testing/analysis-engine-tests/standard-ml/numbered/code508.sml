(* old test case name: code508.sml *)

(* Typable *)
signature s = sig
    structure U : sig type u val f : u -> u end
    structure V : sig type v val x : v end
    sharing type U.u = V.v
end

structure S :> s = struct
structure U = struct type u = int fun f x = x end
structure V = struct type v = U.u val x = 1 end
end;
