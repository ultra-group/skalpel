(* old test case name: code20.sml *)

(* "of" is in the slice because it constraints the type of "D" to be an arrow type *)
let
    datatype t = D of int
in D + 2
end;
