(* old test case name: code67.sml *)

(* type errors involving records *)
fun f x y z = (z x, z y)
val ex1 = (fn {x = x, y = y, ...} => f x y) {x = true, y = 1}
val ex2 = (fn {x = x, y = y, ...} => x + y) {x = true, z = 1, v = 1, z = 1};

(* this is typable *)
(fn x => x) : 'a w;

(* type errors involving type declarations *)
datatype ('a, 'b) t1 = T of int -> 'a | U of bool -> 'b
type 'a t2 = (bool, 'a) t1
type t3 = int t2
type t4 = {x : t3, y : t3}
fun f x = x + 1
fun g x = x + 2
val x : t4 = {x = T f, y = T g}
