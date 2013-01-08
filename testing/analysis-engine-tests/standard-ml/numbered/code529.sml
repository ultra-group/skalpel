(* old test case name: code529.sml *)

(* Untypable because of the value polymorphism restriction.
 * The error goes through a chain of bindings. *)

val foo = {f = (fn x => x) (fn x => x)}
val {f} = foo
val _ = (f 1, f true);
