let
    val g = fn _ => ()
    val f = fn _ => g
in f (f 1) 2
end
