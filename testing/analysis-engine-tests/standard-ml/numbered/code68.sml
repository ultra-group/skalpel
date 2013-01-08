(* old test case name: code68.sml *)

let
    datatype u = U
    datatype v = V
in
    let
	datatype t = T of u
    in T V
    end
end;
