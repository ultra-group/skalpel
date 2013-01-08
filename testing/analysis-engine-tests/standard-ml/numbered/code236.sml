(* old test case name: code236.sml *)

functor F (S : sig val x : int end) = struct val y = S.x end;
