(* old test case name: code58.sml *)

(* untypable - diverse syntactic errors *)
let
    fun gg (g, g x, g) = g
    datatype t = f of bool
    fun f () = 1
    and f 1 = 1
      | f x = x + 1
in fn (f, f y) => fn (g x) => x + 1
end;
