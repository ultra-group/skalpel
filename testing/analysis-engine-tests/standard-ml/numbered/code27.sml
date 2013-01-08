(* old test case name: code27.sml *)

let
    val rec f = fn x => f (f 1, x)
in f 2
end;
