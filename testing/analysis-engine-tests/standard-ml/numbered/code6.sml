(* old test case name: code6.sml *)

let
    fun f (x :: _) (y :: _) z = (z x, z y)
in f [1, 2] [(), 4]
end;
