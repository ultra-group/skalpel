(* old test case name: code79.sml *)

(* the datatype does not participate to any error because
   the status of f is overridden by the fun declaration *)
datatype t = f;
fun f x = x + 2
val f = fn f as (x, y) => #1 f + 1;
f (1, 2);
