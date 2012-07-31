(* old test case name: code514.sml *)

(* Untypable because /\'a.'a is not a type name. *)
signature s = sig datatype 'a t = T end where type 'a t = 'a;
