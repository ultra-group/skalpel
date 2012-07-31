(* old test case name: code438.sml *)

(* Untypable *)
datatype t = T of string
datatype u = T of bool
signature s = sig datatype t = datatype t end
structure S :> s = struct datatype t = datatype u end
