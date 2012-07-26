(* An error reports that t cannot be both:
 * - a datatype in the signature
 * - a type in the structure *)
signature S = sig datatype t = T1 of int | T2 of bool | T3 type u end;
structure st1 :> S = struct type t = int end;
