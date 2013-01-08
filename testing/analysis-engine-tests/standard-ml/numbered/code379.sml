(* old test case name: code379.sml *)

(* untypable *)
signature s = sig
    structure X : sig type t end
end where type X.t = bool

structure S1 :> s = struct
structure X = _structX
val v = 1 : X.t
end;
