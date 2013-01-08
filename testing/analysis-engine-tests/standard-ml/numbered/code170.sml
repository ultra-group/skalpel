(* old test case name: code170.sml *)

(* this is untypable *)
datatype t = C of int | x of int;
val _ = fn (x u) => C u | (C u) => C u;
structure S :> sig datatype t = C of int val x : int -> t val y : int -> t end =
struct datatype t = C of int val x = fn u => C u val y = C end;
val (S.x u) = S.x 1;
