(* old test case name: code41.sml *)

datatype t = T
and 'a t = T | T of ('a, 'a, 'a) w
and 'a u = U of ('a, 'a) t;
