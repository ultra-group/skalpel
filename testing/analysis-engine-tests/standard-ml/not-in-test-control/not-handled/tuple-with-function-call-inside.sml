datatype 'a mydt = firstCons of 'a | secondCons of 'a;

fun test () = 5.0;

val x = firstCons (1, test (), 5);
val y = firstCons (2, test (), 6);

x = y
