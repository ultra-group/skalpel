(* old test case name: code467.sml *)

(* Untypable - value polymorphism restriction through a structure *)
structure S = struct
val f = (fn x => fn y => y) ()
end
val _ = (S.f 1, S.f true);
