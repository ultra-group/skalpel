(* old test case name: code549.sml *)

(* Untypable. *)

functor F (S : sig val x : int end) = struct open S val y = x + 1 end
structure T = F(struct val x = true end);
