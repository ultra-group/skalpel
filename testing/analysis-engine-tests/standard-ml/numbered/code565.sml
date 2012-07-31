(* old test case name: code565.sml *)

(* Typable *)

functor F (S : sig val f : 'a -> 'a end) =
struct structure T = S :> sig val f : int -> int end end;
