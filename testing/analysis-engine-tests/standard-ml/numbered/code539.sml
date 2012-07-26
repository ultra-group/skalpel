(* Untypable.
 *
 * Example where a specification is involved in an error without
 * having the corresponding declaration in the structure involved. *)

signature s = sig val x : int val y : bool end
structure S : s = struct val x = 1 val y = true end
structure T :> s = struct val x = 1 val y = true end
val u = let open T
	    val z = y
	    open S
	in fn w => (w z, w x)
	end
