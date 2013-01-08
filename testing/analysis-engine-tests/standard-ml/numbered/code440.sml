(* old test case name: code440.sml *)

(* Untypable *)
signature s = sig type t exception e end
structure S1 : s = struct type t = int val e = 1 end
structure S2 : s = struct datatype t = e end
structure S3 : s = struct type t = int fun e () = () end;
