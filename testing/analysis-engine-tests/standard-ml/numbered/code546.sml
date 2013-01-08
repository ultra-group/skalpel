(* old test case name: code546.sml *)

(* Untypable.
 * The identifier y is not declared in S. *)

structure S = struct val x = true val x = () end
val _ = S.y;
