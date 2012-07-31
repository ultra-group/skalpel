(* old test case name: code551.sml *)

(* Untypable.
 * In x's first declaration, `` is an free identifier, while in
 * x's second declaration it's a list of SMLofNJ.quotes.
 * We therefore obtain a type error that involves the two last
 * occurrences of x. *)

(**SML-TES-QUASIQUOTES false*);
Control.quotation := false;

val x = ``
val y = x + 1

(**SML-TES-QUASIQUOTES true*);
Control.quotation := true;

val x = ``
val y = x + 1
