(* Untypable *)

functor F (val x : bool) = struct val y = x end
structure S = F(val x = true)
open S
val z = y + 1
