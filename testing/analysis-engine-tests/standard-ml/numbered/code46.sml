(* old test case name: code46.sml *)

let
    val rec f = fn () => 1
    val g1 = fn (f x) => x
    val g2 = fn (f, f) => 1
    val g3 = fn f => f
    val g4 = fn (f x, f y) => (x, y)
    val g5 = fn (f x, f) => x
in f
end;
