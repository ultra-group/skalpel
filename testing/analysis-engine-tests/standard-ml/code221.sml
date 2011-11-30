(* example of how you can come up with value identifiers with any type
   you like, as long as you don't care if it is dead code that can
   never actually be invoked *)
functor F1 (S1 : sig val y : 'a end) = struct
  functor F2 (S2 : sig val y : 'a end) = struct
    val y = S2.y
    val _ = if y then y + 1 else 2
  end
  structure T = F2 (S1)
end
