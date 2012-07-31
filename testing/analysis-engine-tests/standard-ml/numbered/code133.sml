(* old test case name: code133.sml *)

datatype t = f of int
fun f () = let val f = fn x => x in f 1 end
val f = fn x => x
