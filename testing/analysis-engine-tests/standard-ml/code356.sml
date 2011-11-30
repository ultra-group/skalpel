(* Untypable *)
signature s = sig
    type t
    structure X : sig structure Y : sig type t end end
    val v : t
    val f : t -> t -> t
end where type X.Y.t = bool (* typable with int *)

structure S1 :> s = struct
type t = int
structure X = struct structure Y = struct type t = t end end
val v = 1
fun f (x : X.Y.t) y = x + y + v
end
