(* old test case name: code319.sml *)

(* Untypable.
 * This example was sent by Brian1978 to the SML/NJ mailing list. *)
fun two f x = f(f x);
fun five f x = f(f(f(f(f x))));
fun I x = x;

(((fn z =>
      (fn x =>
	  (((z x) x) x))
	  (fn y =>
	      (( two
		     (fn x =>
			 (y(x I))))
		   five)))
      I)
     I);;
