(* Untypable *)

functor F () :> sig val f : 'a -> 'a end = struct fun f x = x end

structure S = F()

val _ = (S.f false) : int
