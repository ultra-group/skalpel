(* old test case name: code389.sml *)

(* untypable *)
signature s = sig
    type t
    structure X : sig
	structure Y : sig type t val x : t end
    end
    val v : t
    val f : t -> t -> t
end where type X.Y.u = bool;
