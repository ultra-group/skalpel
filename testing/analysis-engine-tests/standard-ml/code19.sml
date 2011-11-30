(* This might not be an error if, for example, + is overloaded *)
let
    val rec g = fn _ => 1 + u
in if u then g () else g ()
end
