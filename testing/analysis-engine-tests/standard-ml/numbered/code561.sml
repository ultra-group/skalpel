(* old test case name: code561.sml *)

(* Untypable *)

functor F (val x : ((int -> int) * bool)) =
	struct val _ = #1 x (#2 x) end
structure T = F(val x = (raise Fail, true))
