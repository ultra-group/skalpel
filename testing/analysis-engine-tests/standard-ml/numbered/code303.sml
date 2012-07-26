(* untypable *)
structure T = struct datatype t = C of int end
fun f2 (T.C true)  = 1
  | f2 (T.C false) = 2
