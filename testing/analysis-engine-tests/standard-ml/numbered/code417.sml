(* old test case name: code417.sml *)

(* Untypable *)
structure S = struct type t = int end
datatype t = T of 'a S.t * S.t
