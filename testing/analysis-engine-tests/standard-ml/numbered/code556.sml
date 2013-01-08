(* old test case name: code556.sml *)

(* This example is similar to testcase 525.
 * It doesn't constrain F's body with a signature.
 * It is also untypable. *)

functor F (type k) = struct
  type key = k
  fun insert (x : key, y : 'a) : 'a = y
end

structure S = F(type k = bool)

fun foo lab = (S.insert (lab, lab)) : int


(* Similar and simpler example: *)

functor F (type k) = struct
  type key = k
  fun id (x : key) = x
end

structure S = F(type k = bool)

fun foo lab = (S.id lab) : int;
