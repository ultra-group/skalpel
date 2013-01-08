(* old test case name: code506.sml *)

(* Untypable *)
signature s = sig datatype 'a t = C of 'a end
structure S : s = _S
val _ = S.C true : unit S.t;
