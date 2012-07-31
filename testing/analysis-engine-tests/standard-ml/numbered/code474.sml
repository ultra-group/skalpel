(* old test case name: code474.sml *)

(* Untypable - Error going through the parameter of a functor *)
functor F (S : sig val f : 'a -> 'a end) = struct
val _ = S.f 1
val _ = S.f ()
end
structure X = struct fun f x = x + 1 end
structure T = F(X)
