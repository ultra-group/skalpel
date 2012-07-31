(* old test case name: code374.sml *)

(* untypable *)
structure S : sig val x : 'a end =
struct exception e val x = raise e end;
