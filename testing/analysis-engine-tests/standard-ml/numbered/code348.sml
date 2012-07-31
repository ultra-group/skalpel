(* old test case name: code348.sml *)

(* Untypable *)
datatype t = T
datatype u = U
structure S : sig type v = t end = struct type v = u end

(* Untypable *)
structure st1 : sig type t end = struct type t = bool end;
1 : st1.t;
