(* old test case name: code174.sml *)

(* this is obviously not typable *)
structure S :> sig datatype t = C of int val x : t end =
struct datatype t = C of int val x = 1 end;
