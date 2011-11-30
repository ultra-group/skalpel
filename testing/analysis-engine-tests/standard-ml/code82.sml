(* the fun and datatype do not conflict *)
fun f x = x
datatype t = f;
f 1;
