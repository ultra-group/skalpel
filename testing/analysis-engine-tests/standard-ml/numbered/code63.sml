(* old test case name: code63.sml *)

(* untypable *)
datatype ('a, 'b) comb = Node of 'b * ('a, 'b) comb | Leaf of 'a
and opcomb = TSOME of (bool, int) comb | TNONE
fun trans1 (Node (x, y)) f =
    let
	val newNode = trans1 y f
    in Node (f y, newNode)
    end
  | trans1 (Leaf x)      _ = Leaf x
fun trans2 (Node (x, y)) f = Node (x, trans2 y f)
  | trans2 (Leaf x)      f = Leaf (f x)
fun f x y = (plus : int -> int -> int) x y
fun g x y = x orelse y
val ex = fn u => fn v => TSOME (trans1 (trans2 (Node (0, Leaf true)) g) f)
