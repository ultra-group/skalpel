(* Untypable
 *
 * Unmatched specifications. *)

structure S = struct val (fool, barr, toto) = (1, 2, 3) end
signature s = sig val foo : int val bar : int end
structure T = S :> s
