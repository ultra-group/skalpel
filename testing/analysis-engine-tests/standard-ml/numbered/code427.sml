(* old test case name: code427.sml *)

(* Untypable *)
structure S = struct datatype t = x end
open S
val _ = fn x => (x 1, x true)

(* Untypable *)
structure S = struct datatype t = x fun x () = () end
open S
val _ = fn x => (x 1, x true);
