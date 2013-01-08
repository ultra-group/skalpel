(* old test case name: code44.sml *)

(* example of code where a merging of multi-occurrence errors could be useful *)
let
    datatype t = f
    val rec f = fn (f, f) => f
    and f = fn (f, f) => f
    and f = fn (f, f) => f
in (f true, f (true, true))
end;
