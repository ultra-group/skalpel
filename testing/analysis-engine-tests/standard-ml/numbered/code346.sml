(* old test case name: code346.sml *)

(* Typable *)
structure S = struct datatype 'a t = g of unit | h of 'a * 'a t end;
val f = S.g ();
val _ = (S.h (1,f), S.h (true,f))
