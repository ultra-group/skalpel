(* old test case name: code31.sml *)

datatype 'a t = T of 'a t | U of ('a, 'a) t;
