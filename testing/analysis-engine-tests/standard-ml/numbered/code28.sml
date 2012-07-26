let
    val rec f = fn x => f (f 1, 2)
in ()
end
