let
    fun f (x :: _) (y :: _) z = (z x, z y)
in f (1 :: 2 :: nil) (() :: 4 :: nil)
end
