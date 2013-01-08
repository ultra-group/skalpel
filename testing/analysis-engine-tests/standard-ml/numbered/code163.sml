(* old test case name: code163.sml *)

(* This is typabale *)
structure T :> sig val x : 'a end =
struct exception e val x = raise e end;
(* This isn't *)
signature S = sig datatype t = C val C : int end;;
