(* Untypable *)

signature s = sig val x : 'a -> 'b end
structure S :> s = struct val x = fn x => x end
