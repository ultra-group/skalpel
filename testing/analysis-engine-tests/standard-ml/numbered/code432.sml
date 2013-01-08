(* old test case name: code432.sml *)

(* Untypable *)
structure S = struct structure T = struct datatype t = C of int end end
datatype t1 = C of bool
datatype u1 = datatype S.T.t
datatype t2 = C of string
datatype u2 = datatype u1
val _ = C ();
