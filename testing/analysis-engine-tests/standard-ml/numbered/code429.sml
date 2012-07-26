(* Untypable *)
structure T = struct datatype t = D end
signature s = sig datatype t = C end
structure S :> s = struct open T end
