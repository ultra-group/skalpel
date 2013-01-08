(* old test case name: code252.sml *)

(* untypable *)
val x = true
local
    val x = 1
in
val x = x + 1
end
val _ : bool = x;
