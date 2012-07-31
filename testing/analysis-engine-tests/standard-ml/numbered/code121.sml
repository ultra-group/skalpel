(* old test case name: code121.sml *)

fun g x y =
    let
	val f = if y
                then fn _ => fn z => z
                else fn z => z
	val u = (f, true)
    in (#1 u) y
    end
