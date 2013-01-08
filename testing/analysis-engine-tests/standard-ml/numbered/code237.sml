(* old test case name: code237.sml *)

(* this is typable because the value declaration does not
   declare any value *)
val _ = (fn _ => fn x : 'a => x) ()
(* this is not typable because the value declaration
   declares a value *)
val u = (fn _ => fn x : 'a => x) ();
