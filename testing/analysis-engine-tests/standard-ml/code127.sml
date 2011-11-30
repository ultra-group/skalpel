datatype t = c
fun f c = (c 1, c true)

fun g x = (x, x true)
val ex = g g
