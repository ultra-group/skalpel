(* old test case name: code560.sml *)

(* Untypable *)

structure S = struct val f = fn (x : ('a*'b)) => x end
	      : sig val f : 'a -> 'a end;;
