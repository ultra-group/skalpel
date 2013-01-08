(* old test case name: code457.sml *)

(* EXAMPLE 1 - Untypable -
 * Recursive functions are value variables *)
structure S = struct
  datatype 'a t = T
  datatype 'a u = U
  val rec f = fn x => x
  val rec d1 = fn f =>
	       fn f =>
	       fn f =>
	       fn f => let val rec d2 = fn v => f T
		       in f U
		       end
end

(* EXAMPLE 2 - Nicer version *)
fun f () = ()
val _ = fn f => fn f => fn f => fn f => (f 1, f true);
