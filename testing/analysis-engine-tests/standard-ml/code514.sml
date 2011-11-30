(* Untypable because /\'a.'a is not a type name. *)
signature s = sig datatype 'a t = T end where type 'a t = 'a;
