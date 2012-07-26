datatype t = f | g of int
(* untypable because f is of type t and not unit *)
val f = ()
(* untypable because f is applied in the pattern but defined without 'of' *)
val (f x) = g 1
(* untypable because g is not applied in the pattern but defined with a 'of' *)
val g = fn _ => f
(* untypable because f is not a function, it is of type t *)
val _ = f ()
