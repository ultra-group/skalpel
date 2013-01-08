(* old test case name: code189.sml *)

val f1 = (fn x => x) []
val _ = let val f2 = f1 in f2 + 1 end;
