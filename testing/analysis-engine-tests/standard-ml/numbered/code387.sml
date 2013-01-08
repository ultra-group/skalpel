(* old test case name: code387.sml *)

(* untypable *)
datatype t = f
local
    fun f () = ()
in val f = fn () => () end
val _ = (fn f => f + 1) true

(* untypable *)
datatype t = f
local
    fun g () = ()
in val f = fn () => () end
val _ = (fn f => f + 1) 1

(* typable *)
datatype t = f
local
    fun f () = ()
in val f = fn () => () end
val _ = (fn f => f + 1) 1;
