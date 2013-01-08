(* old test case name: code430.sml *)

(* Untypable *)
structure S = struct structure T = struct datatype t = C of int end end
datatype t = C of bool
open S.T
val _ = C ();
