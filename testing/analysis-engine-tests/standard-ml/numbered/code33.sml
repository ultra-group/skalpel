(* old test case name: code33.sml *)

let
    datatype t = T
in
    let
	datatype u = U
    in
	let
	    val T = U
	in ()
	end
    end
end
