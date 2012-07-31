(* old test case name: code504.sml *)

fun (x + y) z = print (x ^ y ^ z ^ "\n");
val () = ("hello" + "world") "bye";

fun (x + y) = print (x ^ y ^ "\n");
val () = "hello" + "world";

fun x + y = print (x ^ y ^ "\n");
val () = "hello" + "world";
