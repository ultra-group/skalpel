(* old test case name: code528.sml *)

(* Untypable
 *
 * The value identifier f has a monomorphic type and should be
 * assigned a fixed type. *)

structure X : sig val f : 'a -> 'a end = struct
fun g () = fn x => x
val f = g ()
end
val _ = (X.f 1, X.f true)
