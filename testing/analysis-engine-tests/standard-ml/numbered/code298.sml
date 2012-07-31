(* old test case name: code298.sml *)

(* EXAMPLE1:
 * Untypable because the constructor S.C in the pattern is not applied *)
structure S = struct datatype t = C of int end
val S.C = S.C

(* EXAMPLE2:
 * Untypable because the constructor e in the pattern is applied *)
exception e
val (e x) = raise e

(* EXAMPLE3:
 * Untypable becaue the constructor e in the pattern is not applied *)
exception e of int
exception e' = e
val e' = e'

(* EXAMPLE4:
 * Typable *)
exception e
val e = e
