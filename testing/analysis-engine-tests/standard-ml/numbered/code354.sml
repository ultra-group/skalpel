(* old test case name: code354.sml *)

(* Typable *)
signature s = sig
    type t
    structure X : sig type t end
    val v1 : t
    val v2 : t
    val f  : t -> t -> t
end where type X.t = bool
