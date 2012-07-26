(* Untypable: + is not overloaded to true *)
val x = true
local
    val x = true
    val y = true
in
val u = x
end
val _ = u + 1
