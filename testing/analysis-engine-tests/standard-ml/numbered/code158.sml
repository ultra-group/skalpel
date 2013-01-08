(* old test case name: code158.sml *)

(* this is an error for SMLNJ and MOSCOWML but not for HAMLET *)
(* it seems that this shouldn't be an error because the type of f is generalised *)
fun f x = let val _ = x : 'a in () end
val _ = f 1
(* however the next example is an error *)
fun f x = let val _ = x : 'a val _ = x + 1 in () end
val _ = f 1;
