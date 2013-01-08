(* old test case name: code471.sml *)

(* Free identifiers *)
let val x = fn _ =>
	       let exception e of 'a
		   exception e' = e
	       in (e, e)
	       end
in ((#1 (x ())) 1, (#2 (x ())) true)
end;

(* Free identifiers *)
let val x = fn _ =>
	       let exception e of 'a
		   exception e' = e
	       in (e, e)
	       end
in ((raise (#1 (x ())) 4)    handle (e x) => x,
    (raise (#2 (x ())) true) handle (e x) => x)
end;

(* Free identifiers *)
let val x = fn _ =>
	       let exception e of 'a
		   exception e' = e
	       in (e, e)
	       end
in ((raise (#1 (x ())) 4)    handle (e x) => x : int,
    (raise (#2 (x ())) true) handle (e x) => x : bool)
end;;
