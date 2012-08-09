datatype 'a t = C of 'a | D of int -> int

val test1 = (fn x => C x) 5
val test2 = (fn x => C x) 5

(* this will not generate an equality type error *)
val _ = test1 = test2

val test3 = (fn x => C x) 5.0
val test4 = (fn x => C x) 5.0

(* this will generate an equality type error *)
(*  * was this what joe was talking about? *)
val _ = test3 = test4
