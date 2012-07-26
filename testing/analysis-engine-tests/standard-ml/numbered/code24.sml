let
    datatype 'a t = T of ('a, 'a, 'a) w
    and 'a u = U of ('a, 'a) t
    and 'a v = V of 'a w
    and 'a c = C of int t * w
    and 'a d = D of (bool) w
in ()
end
