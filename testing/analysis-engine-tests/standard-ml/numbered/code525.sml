(* old test case name: code525.sml *)

(* Untypable
 *
 * Error involving a functor, an opaque signature and a where clause. *)

signature s2 = sig
    type key
    val insert : (key * 'a) -> 'a
end

functor F (type k) :> s2 where type key = k = struct
  type key = k
  fun insert (x, y) = y
end

structure S = F(type k = bool)

fun foo lab = (S.insert (lab, lab)) : int
