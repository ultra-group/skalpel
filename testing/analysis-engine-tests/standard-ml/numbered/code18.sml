(* old test case name: code18.sml *)

let
    val rec f = fn _ => true
in
    let
	val rec g = fn _ => f ()
    in
	let
	    val rec f = fn _ => g ()
	in f () + 1
	end
    end
end
