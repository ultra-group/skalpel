(* old test case name: code285.sml *)

(* more infix operators *)
val _ = 1 before 1
val _ = 1 <> true orelse 1 > true
(* error with options *)
val _ = (valOf (SOME true)) + 1;
