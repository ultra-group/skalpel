(* old test case name: code487.sml *)

(* Untypable - This is to use with the basis becaus of the
 * use of map.  The error is that we add 1 to each of the
 * elements in the list x which are in fact SMLofNJ.frag's. *)

(**SML-TES-QUASIQUOTES true*)

val c = "C"
val x = `A \/ B \/ ^(c)`
fun m f = map f x
val _ = m (fn x => x + 1);
