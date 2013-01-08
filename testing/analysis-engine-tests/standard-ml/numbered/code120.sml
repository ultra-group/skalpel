(* old test case name: code120.sml *)

datatype t = y
fun g  x y =
    let
	val f =
            if y
	    then fn z => z + 1
            else fn z => z
    in f y
    end;
