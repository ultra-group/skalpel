(* old test case name: code519.sml *)

(* Untypable - S does not define nor*)
structure S = struct
fun not true  = false
  | not false = true
end

val _ = S.nor true;
