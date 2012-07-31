(* old test case name: code286.sml *)

(* application and argument sould be in the slice but not the function (in '(h f)') *)
datatype t = f of int | g | h of int;
val (h f) = fn _ => g and u = 1;
