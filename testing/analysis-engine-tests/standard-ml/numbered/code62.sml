(* old test case name: code62.sml *)

datatype ('a, 'b) comb = Node of 'b * ('a, 'b) comb | Leaf of 'a
and opcomb = TSOME of (bool, int) comb | TNONE
fun trans1 (Node (x, t)) f = Node (x, trans1 t f)
  | trans1 (Leaf x) f      = Leaf (f x)
fun trans2 (Node (x, t)) f = Node (f x, trans2 t f)
  | trans2 (Leaf x)      _ = Leaf x
val trans = fn x => fn f => fn g => trans1 (trans2 x f) f
val f1 = fn x => fn y => x + y
val f2 = fn x => fn y => x orelse y
val ex = fn u => fn v => TSOME (trans (Node (0, Leaf true)) (f1 u) (f2 v));
