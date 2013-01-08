(* old test case name: code564.sml *)

(* Untypable *)

functor F (type t) :> sig val f : t -> t end = struct fun f x = x end;
structure S = F(type t = int);
val _ = S.f true;;
