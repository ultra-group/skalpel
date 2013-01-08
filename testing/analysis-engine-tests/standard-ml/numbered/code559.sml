(* old test case name: code559.sml *)

(* Untypable *)

functor F (val x : bool) = struct val y = x end
structure S = F(val x = true)
open S
val z = y + 1;
