(* old test case name: code34.sml *)

(*(fn (x, x) => (x 1, x true)) ((fn x => x), (fn x => x))*)
(*fun f 1 = f 2 | f x = f true*)
fun not true = false | not false = true
val v = 2
fun f 1 = f 2 | f x = f (not x)
