(* old test case name: code409.sml *)

(* Untypable *)
val _ = fn z => let val x = fn y => z y in (x 1, x true) end
(* Typable *)
val _ = fn z : 'a => fn x : 'a => 1
