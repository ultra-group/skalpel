(* old test case name: code169.sml *)

(* this is untypable *)
datatype t = C of int; val x = C; val (x y) = C 1;
(* this is typable *)
structure S :> sig datatype t = C of int val x : int -> t val y : int -> t end =
struct datatype t = C of int val x = fn x => C x val y = C end;
val (S.x u) = S.x 1;
