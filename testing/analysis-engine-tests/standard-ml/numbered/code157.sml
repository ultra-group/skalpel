(* old test case name: code157.sml *)

(* 'a can't be generalised for x but can be for y *)
val x = let val _ = () in fn x : 'a => x end
and y = fn x : 'a => x
