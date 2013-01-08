(* old test case name: code475.sml *)

(* Untypable, but typable when replacing true by 1. *)
functor F (S : sig val f : int -> int end) : sig val g : int -> int end = struct
val _ = S.f 1
val g = S.f
end
structure X = struct fun f x = x + 1 end
structure T = F(X)
val _ = T.g true

(* Similar but with specifications instead of a structure and its
 * signature.  As for the previous example, it's untypable. *)
functor F (val f : int -> int) : sig val g : int -> int end = struct
val _ = f 1
val g = f
end
structure X = struct fun f x = x + 1 end
structure T = F(X)
val _ = T.g true

(* Similar example, but here g is not accessible in T. *)
functor F (val f : int -> int) : sig end = struct
val g = f
end
structure X = struct fun f x = x + 1 end
structure T = F(X)
val _ = T.g true;
