(* old test case name: code472.sml *)

(* Untypable -
 * Type constructor clash through the parameter of a functor *)
functor F (S : sig val f : 'a -> 'a end) = struct
val _ = S.f () + 1
end
