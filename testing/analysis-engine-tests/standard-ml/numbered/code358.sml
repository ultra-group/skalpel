(* old test case name: code358.sml *)

overload Foo (int, bool)
overload f : 'a -> bool with 'a in (in Foo, unit)
(* Typable: *)
val _ = f 1
(* Untypable: *)
val _ = f 1.1
