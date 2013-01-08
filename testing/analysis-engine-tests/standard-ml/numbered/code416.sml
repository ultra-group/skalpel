(* old test case name: code416.sml *)

(* Untypable *)
datatype 'a t = N of 'a | C of 'a * 'a t
val _ = C (1, N true);
