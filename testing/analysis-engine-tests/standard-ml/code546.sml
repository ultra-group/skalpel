(* Untypable.
 * The identifier y is not declared in S. *)

structure S = struct val x = true val x = () end
val _ = S.y
