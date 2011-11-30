let
    datatype ('a,'b) T = C of ('a * 'b)
    val rec f = fn (C (x,_)) => fn (C (y,_)) => fn z => (z x, z y)
in f (C (1,2)) (C ((),u))
end
