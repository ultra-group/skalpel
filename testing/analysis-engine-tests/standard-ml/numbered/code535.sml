(* old test case name: code535.sml *)

(* Typable *)

datatype t = T1 of t | T2 of int
datatype u = U1 of u | U2 of int

fun d x = x

fun foo c d =
    let val x = d 1
    in c x
    end

val x1 = foo T1 T2
val x2 = foo U1 U2
