(* old test case name: code264.sml *)

(* This is not valid in the Definition and consequently not valid using HaMLet *)
(* However, it is perfectly valid code using SML/NJ *)
val 'a x = let val 'a y = 1 in y end;;
