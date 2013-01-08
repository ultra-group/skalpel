(* old test case name: code57.sml *)

let
    datatype t = f
    fun f f f = f
      | f g h = g
    and f (f, f) = f
in (f true, f (true, true))
end;
