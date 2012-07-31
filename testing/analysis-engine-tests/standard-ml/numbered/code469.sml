(* old test case name: code469.sml *)

(* Untypable *)
let exception e
    val x = raise e
    val f = (fn _ => x) ()
    val _ = (f 1, f true)
in ()
end;

(* Similarly: *)
structure S = struct
val x = let exception e
	    val x = raise e
	    val f = (fn _ => x) ()
	in f
	end
end
val _ = (S.x 1, S.x true)
