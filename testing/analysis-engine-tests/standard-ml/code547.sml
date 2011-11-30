(* Untypable *)

val x = 1
local
    val x = true
in
val y = x
end
val _ = fn z => (z y, z x)
