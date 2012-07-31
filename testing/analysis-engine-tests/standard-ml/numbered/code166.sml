(* old test case name: code166.sml *)

(* parsing error
 * 'fn' should be 'val' *)
val x = 1
fn y = x
val _ = if y then 1 else 2
