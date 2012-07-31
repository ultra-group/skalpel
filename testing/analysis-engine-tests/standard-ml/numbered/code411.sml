(* old test case name: code411.sml *)

(* Untypable *)
overload FOO (int, bool)
overload f : 'a -> bool with 'a in (in FOO)
val _ = f ()
