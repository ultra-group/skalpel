(* Typable *)
signature s = sig
    structure X : sig val f : 'a -> 'a end
end

structure S :> s = struct
structure X = _structX
val _ = X.f 1
end
