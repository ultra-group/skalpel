(* old test case name: code521.sml *)

(* Untypable.
 * Type constructor clash through the signature of a functor.
 *
 * The code would be typable if F's signature was
 * translucent because _F could be replaced by:
 *   struct type 'a map = unit fun insert _ = () end
 *)

signature s2 = sig
    type 'a map
    val insert : ('a map * int * 'a) -> 'a map
end

functor F () :> s2 = _F

structure S = F()

datatype t = UPD of int S.map

fun foo lab sets = UPD (S.insert (sets, lab, (lab, false)))
