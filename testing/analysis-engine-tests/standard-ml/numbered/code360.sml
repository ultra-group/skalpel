(* old test case name: code360.sml *)

(* Untypable - without basis *)
datatype 'a option = NONE | SOME of 'a
fun NONE () = ()
fun seqWith f (p1, p2) getc strm =
    (case (p1 getc strm)
      of SOME(t1, strm1) =>
	 (case (p2 getc strm1)
	   of SOME(t2, strm2) => SOME(f(t1, t2), strm2)
	    | NONE => NONE
	 (* end case *))
       | NONE => NONE
    (* end case *))
fun seq (p1, p2) = seqWith (fn x => x) (p1, p2);
