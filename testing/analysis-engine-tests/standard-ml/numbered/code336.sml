(* old test case name: code336.sml *)

overload + : 'a * 'a  -> 'a with 'a in (int, real, word)
(* untypable *)
fun f x y = x + (y : 'a)
(* typable *)
fun f x y = x + (y : int)
