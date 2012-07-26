signature s = sig
    datatype 'a t = C
    val f : 'a t -> bool
end

(* Untypable - t is specified as a datatype but is not
 * a type name in where clause. *)
structure S :> s where type 'a t = int = _structS
val _ = S.f 1

(* Untypable - Similar to above plus the error that
 * order clashes with int. *)
structure S :> s where type 'a t = order = _structS
val _ = S.f 1
