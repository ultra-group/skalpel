(* old test case name: code353.sml *)

signature s = sig
    type t
    structure X : sig type t end
    val v1 : t
    val v2 : t
    val f  : t -> t -> t
end

structure S1 :> s = struct
type t = bool
structure X = struct type t = t end
val v1 = true
val v2 = false
fun f (x : X.t) y = x andalso y
end

structure S2 :> s = struct
type t = bool
structure X = struct type t = t end
val v1 = true
val v2 = false
fun f (x : X.t) y = x orelse y
end

(* Untypable because the signatures are opaque. *)
val _ = S1.f S2.v1;
