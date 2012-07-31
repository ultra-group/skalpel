(* old test case name: code277.sml *)

(* SML/NJ reports:
 * ... Error: datatype t does not match specification
 * constructors in actual only: D *)
signature s = sig datatype t = C val D : t end
structure S :> s = struct datatype t = C | D end
(* this is OK *)
signature s' = sig type t val C : t val D : t end
structure S' :> s' = struct datatype t = C | D end
