(* old test case name: code532.sml *)

(* Untypable.
 *
 * Same as 523 without the where clause. *)

signature s1 = sig
    type key
end

signature s2 = sig
    structure Key : s1
    type 'a map
    val insert : ('a map * Key.key * 'a) -> 'a map
end

functor F (K : s1) :> s2 = struct
  structure Key = struct
    type key = K.key
  end
  type 'a map = unit
  fun insert _ = ()
end

structure S = F(type key = int)

datatype t = UPD of int S.map

fun foo lab sets = UPD (S.insert (sets, lab, lab));
