(* Typable *)
signature s = sig type t val e : t end
structure S1 : s = struct type t = int val e = 1 end
structure S2 : s = struct datatype t = e end
