(* i managed to generate an error for this by using consTYPE_VARwithEQ...
 * but it doesn't work for types, and only datatypes. If I can restrict that somehow to
 * work with datatypes only, this should pass *)

datatype 'a t = C of int | D of bool;
(fn x : real t => fn y : real t => x = y);

(* perhaps this untypable program will help with debugging... *)
(* datatype 'a t = C of int | D of bool; *)
(* (fn x : int t => fn y : real t => x = y); *)
