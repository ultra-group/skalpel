(* Untypable *)
val _ = let val x = fn _ : 'a => let exception e of 'a in e end
	in ((raise x 1 1)       handle (e x) => x + 1;
	    (raise x true true) handle (e x) => x orelse true;
	    1) end
