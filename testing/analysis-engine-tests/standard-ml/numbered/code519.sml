(* Untypable - S does not define nor*)
structure S = struct
fun not true  = false
  | not false = true
end

val _ = S.nor true
