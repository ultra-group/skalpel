(* old test case name: code52.sml *)

let
    datatype ('a,'b) T = C of ('a * 'b)
    fun f (C (x,_)) (C (y,_)) z = (z x, z y)
in f (C (1,2)) (C ((),u))
end
