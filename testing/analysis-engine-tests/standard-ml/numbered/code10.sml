(* old test case name: code10.sml *)

(* Our simpler solution to handle the Standard ML Basis Library
   does handle List but not Option *)
datatype 'a List = Nil | Cons of ('a * ('a List))
and 'a Option = None | Some of 'a
fun f Nil             = None
  | f (Cons (x, Nil)) = x (*Some x*)
  | f (Cons (_, l))   = f l
val ex = f (Cons (1, Cons (2, Nil)))

(* This shouldn't be typable *)
datatype ('a, 'b) mytree = Node of ('a, 'b) w * 'b | Leaf of 'a
and treeoption = TSOME of (int, bool) w | TNONE
fun g (Node (x, y)) = g x
  | g (Leaf x)      = x
val x = TSOME (g (Node (Leaf 0, true)))
