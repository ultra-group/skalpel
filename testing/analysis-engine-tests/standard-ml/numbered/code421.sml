(* old test case name: code421.sml *)

(* Untypable *)
overload c : 'a -> 'a -> 'a with 'a in (int, word, real, string)
fun f x = (fn x => c x 0w0) (x : int)
(* Untypable *)
overload c : 'a -> 'a -> 'a with 'a in (_typet)
val _ = c 1 0w0
(* Untypable *)
overload c : 'a -> 'a -> 'a with 'a in (bool)
val _ = c 1 0w0;
