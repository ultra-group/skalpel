(* Untypable *)
signature s = sig type 'a t = bool end
structure S :> s = struct type 'a t = 'a end
