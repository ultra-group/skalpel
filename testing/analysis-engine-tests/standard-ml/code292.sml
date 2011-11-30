functor F (S : sig val u : 'a val + : 'a end) = struct open S; val _ =
let
    val rec g = fn _ => 1 + u
in if u then g () else g ()
end
end
