(* old test case name: code51.sml *)

let
    datatype ('a,'b) T = C of ('a * 'b) | D of ('a * 'b)
    val rec f = fn (C (x,_)) => fn (D (y,_)) => fn z => (z x, z y)
in f (C (1,2)) (D ((),u))
end
