(* old test case name: code386.sml *)

(* untypable *)
local
    fun f () = ()
    val g = fn () => []
    val f = g ()
in
    val f = g ()
end

fun g () = []
val f = g ()
val _ = (1 :: f, true :: f)
