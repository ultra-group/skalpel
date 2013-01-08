(* old test case name: code50.sml *)

let
    datatype t = f of int
    val f = 3
in fn (f, f y) => (f 3, y)
end;
