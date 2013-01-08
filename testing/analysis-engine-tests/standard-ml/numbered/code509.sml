(* old test case name: code509.sml *)

(* Untypable *)

(**SML-TES-SPEC
    val foo : string -> string
    val bar : string -> string
 *)

fun f x = foo x
val _ = bar ((f "") + 1);
