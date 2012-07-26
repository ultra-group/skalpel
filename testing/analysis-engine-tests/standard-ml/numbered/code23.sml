let
    datatype ('a, 'b) t = T of ('a, 'a, 'b) t
    and m = M
    and 'a u = U of 'a t
in ()
end
