(* Untypable.
 *
 * This examples is to use with the basis file.
 * It shows a type constructor clash through a quasiquote.
 * The same error could be obtain using a list instead. *)

(**SML-TES-QUASIQUOTES true*)

val _  = let datatype frag = datatype SMLofNJ.frag
	     val x = 15
	     val y = #"a"
	 in foldr (fn (QUOTE s, r) => (String.size s) :: r
		    | (ANTIQUOTE a, r) => (a * a) :: r)
		  []
		  `this is ^x(a) ^(x * x)abc^(y)def`
	 end;
