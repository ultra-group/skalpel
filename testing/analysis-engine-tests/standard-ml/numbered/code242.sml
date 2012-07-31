(* old test case name: code242.sml *)

(* This should fail under the context dependency that SIG declares t *)
structure Str : SIG = struct type t = bool end;
33 : Str.t;
