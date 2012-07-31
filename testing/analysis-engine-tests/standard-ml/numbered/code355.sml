(* old test case name: code355.sml *)

(* Untypable *)
signature s = sig
    type t
    structure X : sig type t end
    val v : t
    val f : t -> t -> t
end where type X.t = bool (* typable with int *)

structure S1 :> s = struct
type t = int
structure X = struct type t = t end
val v = 1
fun f (x : X.t) y = x + y + v
end
