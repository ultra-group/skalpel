signature s = sig end
signature t = sig structure S : s end
(* this is typable *)
structure S = struct structure S = struct end end : t : s
(* this is untypable *)
structure S = struct structure S = struct end end : s : t
