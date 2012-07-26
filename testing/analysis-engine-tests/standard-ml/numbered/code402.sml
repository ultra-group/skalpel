(* Untypable *)
datatype t = C
structure S = struct end
open S
overload f : 'a -> int with 'a in (t)
val _ = f 1
