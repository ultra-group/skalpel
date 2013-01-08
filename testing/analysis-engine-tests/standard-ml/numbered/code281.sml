(* old test case name: code281.sml *)

(* error under the context dependency that x is not an exception constructor *)
val x = 2
exception f = x

(* For now the context dependency is not quite right.
 * we should have only exception. *);
