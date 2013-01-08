(* old test case name: code423.sml *)

(* Untypable.  To type check with the basis*)
fn x => (fn x => x+2) (x : string);
