(* old test case name: code267.sml *)

(* SML/NJ does not allow that, HaMLet does *)
 type t = int; datatype uu = datatype t; val _ = 1 : uu;
