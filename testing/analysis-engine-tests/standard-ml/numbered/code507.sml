(* old test case name: code507.sml *)

(* Untypable *)
signature s = sig datatype 'a t = C of 'a end where type 'a t = bool
structure S : s = _S
val _ = S.C true : unit S.t
