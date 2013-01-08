(* old test case name: code483.sml *)

(* Untypable - Because C's signature is opaque, its type s has to be
 * different from the s defined in B which is the type of s in S. *)
structure B :> sig
    type s
    type c
end = _B

open B

signature C = sig
    type c
    type s
    val toString : c -> s
end

structure C :> C where type c = c = _C

signature S = sig
    type c
    type s
    val translate : (c -> s) -> s -> s
end

structure S :> S where type s = s where type c = C.c = _S

val _ = fn x => S.translate (fn y => C.toString x);
