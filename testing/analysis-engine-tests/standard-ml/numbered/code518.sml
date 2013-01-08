(* old test case name: code518.sml *)

(* Untypable, T does not define the type t *)
structure T = struct type u = int end
structure S : sig exception e of T.t end = struct exception e of int end

(* Similar to the above example, but with a datatype *)
structure T = struct type u = int end
structure S : sig datatype d = D of T.t end = struct datatype d = D of int end;
