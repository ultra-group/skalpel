(* Typable *)
fun f (x : Int32.int) y = x + y;
fun f (x : LargeWord.word) y = x + y;
(* Untypable *)
fun f (x : order) y = x + y;
