(* old test case name: code263.sml *)

structure S :> sig val x : bool end = S'
open S;
val _ = 1;
val _ = x + 1
