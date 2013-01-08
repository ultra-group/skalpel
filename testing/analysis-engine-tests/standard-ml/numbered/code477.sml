(* old test case name: code477.sml *)

(* Untypable, because + is not overloaded to quasiquotes and
 * quasiquotes and int are different. *)
1 + `foobar`;
