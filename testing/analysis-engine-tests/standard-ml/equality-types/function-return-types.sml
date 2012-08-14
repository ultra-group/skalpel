fun z y = y;

fun x _ = z 2.0;

val y = x (x (x 5.0));
val a = ((fn _ => 0) (x 3.0));

y = a
