(* old test case name: code28.sml *)

let
    val rec f = fn x => f (f 1, 2)
in ()
end
