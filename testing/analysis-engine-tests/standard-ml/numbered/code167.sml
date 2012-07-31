(* old test case name: code167.sml *)

(* untypable *)
structure S :> sig val y1 : int val y2 : bool val x : 'a val y : bool end =
struct val x = 1 val z = 1 val y = true end;

val _ = (S.y2, S.z, S.x, S.u, S.y + 1)
