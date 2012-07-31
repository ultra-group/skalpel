(* old test case name: code515.sml *)

(* Untypable if SML/NJ quasiquotations turned on. *)

(**SML-TES-QUASIQUOTES true*)

val _ = `foo ^(1) bar ^(true)`
