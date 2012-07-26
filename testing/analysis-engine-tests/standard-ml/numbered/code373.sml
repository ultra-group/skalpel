(* untypable *)
val f = let val x = [] in x end
val g = f;
val _ = (1 :: g, true :: g)

(* typable *)
fun f x = let val y = x () in y end
val _ = (f (fn _ => true), f (fn _ => 1))
