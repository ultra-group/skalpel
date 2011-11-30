(* expansive / non expansive.  Different ways to make an expression expansive *)
let
    val (x1, x2, x3, x4) = ((fn x => x) [], while true do (), u1 u2 u3, v1 (v2 v3))
    val y = 1 :: x1
    val z = true :: x1
in ()
end
