(* old test case name: code495.sml *)

(* Untypable *)
signature s = sig exception e of string end
structure S :> s = struct exception e of string end
open S
val _ = e 1;;
