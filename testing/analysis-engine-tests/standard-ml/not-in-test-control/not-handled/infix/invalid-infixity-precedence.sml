fun infop (y,z) = y + z;
infix 5 infop;

fun infopBool (y,z) = if y then z else ~z;

infix 10 infopBool;
5 infop true infopBool 6;
