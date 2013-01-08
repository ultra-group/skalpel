(* old test case name: code173.sml *)

(* This is typable *)
structure S :> sig datatype t = C of int | D val x : int -> t val y : int -> t end =
struct datatype t = C of int | D val x = fn u => C u val y = C end;
S.D : S.t;
