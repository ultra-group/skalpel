(* old test case name: code61.sml *)

fun f (x : 'a -> bool) = x 1

fun f x = (x : 'a -> bool) 1

fun f (x : 'a) = x
val _ = (f true, f 1)

fun g () =
    let
	fun f (x : 'a) = x
	val _ = (f true, f 1)
    in x : 'a
    end
