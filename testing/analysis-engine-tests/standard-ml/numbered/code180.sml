(* this is OK so far *)
val x = y
val _ = (1 :: x, true :: x)
(* this isn't typable *)
val f = (fn x => x) []
val h = let val g = f in (1 :: g, true :: g) end
(* this is typable *)
val _ = fn x => let val f = (fn x => x)[] in f end
