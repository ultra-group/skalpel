(* old test case name: code550.sml *)

(* Untypable *)

functor F (S : sig val f : 'a -> 'a end) = struct open S val x = f () end
structure X = struct fun f x = x + 1 end
structure T = F(X)
