(* old test case name: code536.sml *)

(* Untypable *)

datatype t = T1 of t | T2 of int
datatype u = U1 of u | U2 of int

fun foo c d =
    let val {x, y} = {x = fn x => x, y = d 1}
	val _ = c y
    in (x 1, x true)
    end

val x1 = foo T1 T2
val x2 = foo U1 U2
