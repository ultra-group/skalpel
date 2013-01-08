(* old test case name: code29.sml *)

(* We don't care about D, and that z is the only argument of D. *)
datatype ('a,'b) t = C of ('a * 'b)
     and 'a u = D of 'a
fun f (C (x,_)) (C (y,_)) (D z) = (z x, z y)
val _ = f (C (1,2)) (C ((),u))
(* We care about V but not about x being the only argument of V. *)
datatype v = V of int
datatype w = W of int
val _ = (fn (V x) => x) (W 1);
