(* Untypable *)
structure S = struct structure T = struct datatype t = C of int end end
datatype t = C of bool
datatype u = datatype S.T.t
val _ = C ()
